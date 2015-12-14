module Text.Dot.Types.Internal (
      module Text.Dot.Types.Internal
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


type GraphName = Text
data GraphType = UndirectedGraph
               | DirectedGraph
    deriving (Show, Eq)

type AttributeName = Text
type AttributeValue = Text
type Attribute = (Text, Text)

data NodeId = UserId Text
            | Nameless Int
    deriving (Show, Eq)

data DecType = DecGraph
             | DecNode
             | DecEdge
    deriving (Show, Eq)

data DotGraph = Graph GraphType GraphName Dot
    deriving (Show, Eq)

data RankdirType = LR
                 | TB
    deriving (Show, Eq)

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

instance Monoid Dot where
    mempty = DotEmpty

    -- Left identity
    mappend DotEmpty d = d

    -- Right identity
    mappend d DotEmpty = d

    -- Associativity
    mappend d (DotSeq d1 d2) = DotSeq (mappend d d1) d2

    mappend d1 d2 = DotSeq d1 d2


