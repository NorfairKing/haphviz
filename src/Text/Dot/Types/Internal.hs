module Text.Dot.Types.Internal (
      module Text.Dot.Types.Internal
    , Identity(..)
    , Monoid(..)
    ) where

import           Control.Monad         (unless)
import           Data.Functor.Identity (Identity (..))
import           Data.Monoid
import           Data.Text             (Text)
import qualified Data.Text             as T

import           Control.Monad.Reader  (ReaderT, ask, runReaderT)
import           Control.Monad.Writer  (WriterT, execWriterT, tell)


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
data Dot = Node NodeId [Attribute]
         | Edge NodeId NodeId [Attribute]
         | Declaration DecType [Attribute]
         | RawDot Text
         | Label Text
         | DotSeq Dot Dot
         | DotEmpty
    deriving (Show, Eq)

instance Monoid Dot where
    mappend d1 d2 = DotSeq d1 d2
    mempty = DotEmpty
