module Text.Dot.Types.Internal where

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
data Dot = Node NodeId [Attribute]
         | Edge NodeId NodeId -- Can only have one type of edge per graph type
         | Declaration DecType [Attribute]
         | RawDot Text
         | Label Text
         | DotSeq Dot Dot
         | DotEmpty
    deriving (Show, Eq)

