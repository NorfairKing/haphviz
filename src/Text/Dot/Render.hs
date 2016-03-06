{-# LANGUAGE OverloadedStrings #-}

-- | Rendering Graphviz code from Haphviz graphs
module Text.Dot.Render (
      renderGraph
    , renderToFile
    , renderToStdOut
    ) where

import           Control.Monad           (unless)
import           Data.Monoid
import           Data.Text               (Text)
import qualified Data.Text               as T
import qualified Data.Text.IO            as T

import           Control.Monad.Identity  (Identity (..))
import           Control.Monad.Reader    (ReaderT, ask, runReaderT)
import           Control.Monad.State     (StateT, execStateT, get, modify)
import           Control.Monad.Writer    (WriterT, execWriterT, tell)

import           Text.Dot.Types.Internal

type Render = ReaderT GraphType (StateT Int (WriterT Text Identity))

-- | Render a given graph and write the result to the given file
renderToFile :: FilePath -> DotGraph -> IO ()
renderToFile file g = T.writeFile file $ renderGraph g

-- | Render a given graph and print it to std out
renderToStdOut :: DotGraph -> IO ()
renderToStdOut = T.putStrLn . renderGraph

-- | Render a graph to graphviz code
renderGraph :: DotGraph -> Text
renderGraph (Graph gtype name content) = mconcat
    [ case gtype of
        DirectedGraph -> "digraph"
        UndirectedGraph -> "graph"
    , " "
    , name
    , " "
    , "{"
    , "\n"
    , runIdentity $ execWriterT $ execStateT (runReaderT (renderDot content) gtype) 1
    , "}"
    ]


renderDot :: Dot -> Render ()

renderDot (Node nid ats) = do
    indent
    renderId nid
    renderAttributes ats
    finishCommand
    newline

renderDot (Edge from to ats) = do
    indent
    renderId from
    tell " "
    t <- ask
    tell $ case t of
        UndirectedGraph -> "--"
        DirectedGraph   -> "->"
    tell " "
    renderId to
    renderAttributes ats
    finishCommand
    newline

renderDot (Declaration t ats) = do
    indent
    renderDecType t
    renderAttributes ats
    finishCommand
    newline

renderDot (Subgraph name content) = do
    indent
    tell "subgraph"
    tell " "
    tell name
    tell " "
    tell "{"
    newline
    indented $ renderDot content
    indent
    tell "}"
    newline

renderDot (RawDot t) = tell t

renderDot (Rankdir t) = do
    indent
    tell "rankdir"
    tell " = "
    renderRankDirType t
    finishCommand
    newline

renderDot (Label t) = do
    indent
    tell "label"
    tell " = "
    tell $ quoted t
    finishCommand
    newline

renderDot (Ranksame d) = do
    indent
    braced $ do
        tell " rank=same"
        newline
        indented $ renderDot d
        indent
    newline

renderDot (DotSeq d1 d2) = do
    renderDot d1
    renderDot d2

renderDot DotEmpty = return ()

renderAttributes :: [Attribute] -> Render ()
renderAttributes ats = unless (null ats) $ do
    tell " "
    tell "["
    commaSeparated $ map renderAttribute ats
    tell "]"
  where
    commaSeparated [] = return ()
    commaSeparated [r] = r
    commaSeparated (r:rs) = do
        r
        tell ", "
        commaSeparated rs

renderAttribute :: Attribute -> Render ()
renderAttribute (name, value) = do
    tell name
    tell "="
    tell $ quoteHtml value

renderId :: NodeId -> Render ()
renderId (Nameless i) = tell $ T.pack $ show i
renderId (UserId t) = tell $ quoted t

renderDecType :: DecType -> Render ()
renderDecType DecGraph = tell "graph"
renderDecType DecNode  = tell "node"
renderDecType DecEdge  = tell "edge"

renderRankDirType :: RankdirType -> Render ()
renderRankDirType TB = tell "TB"
renderRankDirType BT = tell "BT"
renderRankDirType RL = tell "RL"
renderRankDirType LR = tell "LR"

newline :: Render ()
newline = tell "\n"

finishCommand :: Render ()
finishCommand = tell ";"

braced :: Render () -> Render ()
braced content = do
    tell "{"
    content
    tell "}"

indent :: Render ()
indent = do
    level <- get
    tell $ T.pack $ replicate (level * 2) ' '

indented :: Render () -> Render ()
indented func = do
    modify ((+) 1)
    func
    modify (flip (-) 1)

-- | Text processing utilities
quoted :: Text -> Text
quoted t = if needsQuoting t then "\"" <> t <> "\"" else t

quoteHtml :: Text -> Text
quoteHtml t = "<" <> t <> ">"

needsQuoting :: Text -> Bool
needsQuoting = T.any (== ' ')
