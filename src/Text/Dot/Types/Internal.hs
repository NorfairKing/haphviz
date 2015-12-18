-- | Internal types.
--
-- You may want to keep off these.
module Text.Dot.Types.Internal (
      module Text.Dot.Types.Internal
    -- * Re-exports

    , Identity(..)
    , Monoid(..)
    ) where

import           Control.Monad          (unless)
import           Data.Monoid
import           Data.Text              (Text)
import qualified Data.Text              as T

import           Control.Monad.Identity (Identity (..))
import           Control.Monad.Reader   (ReaderT, ask, runReaderT)
import           Control.Monad.Writer   (WriterT, execWriterT, tell)


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
         | Subgraph Text Dot
         | RawDot Text
         | Label Text
         | Rankdir RankdirType
         | DotSeq Dot Dot
         | DotEmpty
    deriving (Show, Eq)

-- | Dot is a monoid, duh, that's the point.
instance Monoid Dot where
    mempty = DotEmpty

    -- Left identity
    mappend DotEmpty d = d

    -- Right identity
    mappend d DotEmpty = d

    -- Associativity
    mappend d (DotSeq d1 d2) = DotSeq (mappend d d1) d2

    mappend d1 d2 = DotSeq d1 d2


