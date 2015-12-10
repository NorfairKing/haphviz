{-# LANGUAGE OverloadedStrings #-}
module Text.Dot.Gen (
      module Text.Dot.Gen
    , Dot
    , DotGraph

    , NodeId
    , Attribute
    ) where

import           Control.Monad.State     (StateT, execStateT, get, modify)
import           Control.Monad.Writer    (WriterT, execWriterT, tell)

import           Text.Dot.Types.Internal

import           Data.Text               (Text)
import qualified Data.Text               as T

type DotGen = StateT State (WriterT Dot Identity)

type State = Int -- Next nameless node number

graph :: GraphType -> GraphName -> DotGen a -> DotGraph
graph gt gn func = Graph gt gn d
  where d = runIdentity $ execWriterT $ execStateT func 0

-- * Graph types

-- | Undirected graph
directed :: GraphType
directed = DirectedGraph

-- | Directed graph
undirected :: GraphType
undirected = UndirectedGraph

-- * Nodes
-- | Most general node declaration
genNode :: Text -> [Attribute] -> DotGen NodeId
genNode t ats = do
    let ni = UserId t
    tell $ Node ni ats
    return ni

-- | Nameless node with attributes
namelessNode :: [Attribute] -> DotGen NodeId
namelessNode ats = do
    ni <- newNameless
    tell $ Node ni ats
    return ni

-- | Node with a label but no other attributes
node :: Text -- ^ Label
     -> DotGen NodeId
node label = namelessNode [("label", label)]


-- Generate a new nameless node ID
newNameless :: DotGen NodeId
newNameless = do
    i <- get
    modify (+1)
    return $ Nameless i


-- * Edges

-- Most general edge declaration
genEdge :: NodeId -> NodeId -> [Attribute] -> DotGen ()
genEdge n1 n2 ats = tell $ Edge n1 n2 ats


-- Infix edge constructor. (No attributes)
(-->) :: NodeId -> NodeId -> DotGen ()
n1 --> n2 = genEdge n1 n2 []










