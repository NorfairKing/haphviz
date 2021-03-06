-- | Internal types.
--
-- You may want to keep off these.
module Text.Dot.Types.Internal (
      module Text.Dot.Types.Internal
    -- * Re-exports

    , Identity(..)
    , Monoid(..)
    ) where

import           Data.Text              (Text)

import           Control.Monad.Identity (Identity (..))


-- | Internal name of a graph, used to reference graphs and subgraphs
type GraphName = Text

-- | Type of a graph, directed or undirected.
--
-- This also specifies what edge declarations look like.
data GraphType = UndirectedGraph
               | DirectedGraph
    deriving (Show, Eq)

-- | Attribute name: just text
type AttributeName = Text

-- | Attribute value: just text
type AttributeValue = Text

-- | Attribute: a tuple of name and value.
type Attribute = (AttributeName, AttributeValue)

-- | A node identifier.
--
-- This is either a user supplied name or a generated numerical identifier.
data NodeId = UserId Text
            | Nameless Int
    deriving (Show, Eq)

-- | Declaration type
--
-- Used to declare common attributes for nodes or edges.
data DecType = DecGraph
             | DecNode
             | DecEdge
    deriving (Show, Eq)

-- | A Haphviz Graph
data DotGraph = Graph GraphType GraphName Dot
    deriving (Show, Eq)

-- | Rankdir Type
--
-- Used to specify the default node layout direction
data RankdirType = LR
                 | RL
                 | TB
                 | BT
    deriving (Show, Eq)

-- | Haphviz internal graph content AST
data Dot = Node NodeId [Attribute]
         | Edge NodeId NodeId [Attribute]
         | Declaration DecType [Attribute]
         | Ranksame Dot
         | Subgraph Text Dot
         | RawDot Text
         | Label Text
         | Rankdir RankdirType
         | DotSeq Dot Dot
         | DotEmpty
    deriving (Show, Eq)

-- | Dot is a semigroup, duh, that's the point.
instance Semigroup Dot where
    -- Left identity
    (<>) DotEmpty d = d

    -- Right identity
    (<>) d DotEmpty = d

    -- Associativity
    (<>) d (DotSeq d1 d2) = DotSeq ((<>) d d1) d2

    (<>) d1 d2 = DotSeq d1 d2

-- | Dot is a monoid, duh, that's the point.
instance Monoid Dot where
    mempty = DotEmpty
