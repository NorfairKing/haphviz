{-# LANGUAGE OverloadedStrings #-}
module Text.Dot.Render (renderGraph) where

import           Control.Monad           (unless)
import           Data.Monoid
import           Data.Text               (Text)
import qualified Data.Text               as T

import           Control.Monad.Identity  (Identity (..))
import           Control.Monad.Reader    (ReaderT, ask, runReaderT)
import           Control.Monad.State     (StateT, execStateT, get, modify)
import           Control.Monad.Writer    (WriterT, execWriterT, tell)

import           Text.Dot.Types.Internal

type Render = ReaderT GraphType (StateT Int (WriterT Text Identity))


renderGraph :: DotGraph -> Text
renderGraph (Graph gtype name content) = mconcat
    [ "digraph"
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
    renderId nid
    renderAttributes ats

renderDot (Edge from to ats) = do
    renderId from
    tell " "
    t <- ask
    tell $ case t of
        UndirectedGraph -> "--"
        DirectedGraph   -> "->"
    tell " "
    renderId to
    renderAttributes ats

renderDot (Declaration t ats) = do
    renderDecType t
    renderAttributes ats

renderDot (Subgraph name content) = do
    tell "subgraph"
    tell " "
    tell name
    tell " "
    tell "{"
    tell "\n"
    indented $ renderDot content
    indent
    tell "}"

renderDot (RawDot t) = tell t

renderDot (Rankdir t) = do
    tell "rankdir"
    tell " = "
    renderRankDirType t

renderDot (Label t) = do
    tell "label"
    tell " = "
    tell $ quoted t

renderDot (DotSeq d1 d2) = do
    ind d1
    renderDot d1
    nl d1
    ind d2
    renderDot d2
    nl d2
  where
    nl :: Dot -> Render ()
    nl (DotSeq _ _) = return ()
    nl DotEmpty = return ()
    nl _ = do
        tell ";"
        tell "\n"

    ind :: Dot -> Render ()
    ind (DotSeq _ _) = return ()
    ind DotEmpty = return ()
    ind _ = do
        level <- get
        tell $ T.pack $ replicate (level * 2) ' '


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
renderDotType DecEdge  = tell "edge"

renderRankDirType :: RankdirType -> Render ()
renderRankDirType TB = tell "TB"
renderRankDirType LR = tell "LR"

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
