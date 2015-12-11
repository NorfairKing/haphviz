{-# LANGUAGE OverloadedStrings #-}
module Text.Dot.Gen (
      module Text.Dot.Gen
    -- * Graph Types
    , Dot
    , DotGraph
    , NodeId
    , Attribute
    , DecType
    , RankdirType
    ) where

import           Control.Monad.State     (StateT, execStateT, get, modify)
import           Control.Monad.Writer    (WriterT, execWriterT, tell)

import           Text.Dot.Types.Internal

import           Control.Monad           (void)
import           Data.Monoid             (Monoid (..), (<>))

import           Data.Text               (Text)
import qualified Data.Text               as T

type DotGen = StateT State (WriterT Dot Identity)

type State = Int -- Next nameless node number

-- | Generate graph
graph :: GraphType
      -> GraphName -- ^ Graph name
      -> DotGen a  -- ^ Content
      -> DotGraph
graph gt gn func = Graph gt gn $ genDot func

graph_ :: GraphType
       -> DotGen a -- ^ Content
       -> DotGraph
graph_ gt func = graph gt "haphviz" func

-- | Generate Internal dot AST
genDot :: DotGen a -> Dot
genDot = genSubDot 0

genSubDot :: Int -> DotGen a -> Dot
genSubDot n func = runIdentity $ execWriterT $ execStateT func n

-- * Graph types

-- | Undirected graph
directed :: GraphType
directed = DirectedGraph

-- | Directed graph
undirected :: GraphType
undirected = UndirectedGraph

-- * Nodes
-- | Most general node declaration
genNode :: NodeId -> [Attribute] -> DotGen ()
genNode ni ats = tell $ Node ni ats

-- | Node with given name and attributes
namedNode :: Text -- ^ Name
          -> [Attribute] -> DotGen NodeId
namedNode t ats = do
    let ni = UserId t
    genNode ni ats
    return ni

-- | Nameless node with attributes
namelessNode :: [Attribute] -> DotGen NodeId
namelessNode ats = do
    ni <- newNode
    genNode ni ats
    return ni

-- | Node with a label but no other attributes
node :: Text -- ^ Label
     -> DotGen NodeId
node l = namelessNode [label =: l]

-- | Node with given node Id and label
node_ :: NodeId -- ^ given Node ID
      -> Text -- ^ Label
      -> DotGen ()
node_ ni l = genNode ni [label =: l]

-- Generate a new nameless node ID
newNode :: DotGen NodeId
newNode = do
    i <- get
    modify (+1)
    return $ Nameless i


-- * Edges

-- | Most general edge declaration
genEdge :: NodeId -> NodeId -> [Attribute] -> DotGen ()
genEdge n1 n2 ats = tell $ Edge n1 n2 ats


-- | Infix edge constructor. (No attributes)
(-->) :: NodeId -> NodeId -> DotGen ()
n1 --> n2 = genEdge n1 n2 []

-- * Attributes

-- | Infix operator for an attribute pair
(=:) :: AttributeName -> AttributeValue -> Attribute
(=:) = (,)

-- ** Attribute Names

label :: AttributeName
label = "label"

compound :: AttributeName
compound = "compound"

shape :: AttributeName
shape = "shape"

color :: AttributeName
color = "color"

dir :: AttributeName
dir = "dir"

width :: AttributeName
width = "width"

height :: AttributeName
height = "height"

-- ** Attribute values

true :: AttributeValue
true = "true"

false :: AttributeValue
false = "false"

none :: AttributeValue
none = "none"


-- * Declarations

-- | General declaration of attributes
genDec :: DecType -> [Attribute] -> DotGen ()
genDec t ats = tell $ Declaration t ats

-- | Graph declaration
--
-- >>> graphDec [compound =: true]
-- > graph [compound=true];
graphDec :: [Attribute] -> DotGen ()
graphDec = genDec DecGraph

-- | Node declaration
--
-- >>> nodeDec [shape =: none]
-- > node [shape=none];
nodeDec :: [Attribute] -> DotGen ()
nodeDec = genDec DecNode

-- | Edge declaration
--
-- >>> edgeDec [color =: "red:blue"]
-- > edge [color="red:blue"];
edgeDec :: [Attribute] -> DotGen ()
edgeDec = genDec DecEdge


-- * Subgraphs

-- | Subgraph
subgraph :: Text -> DotGen () -> DotGen GraphName
subgraph name content = do
    n <- get
    let c = genSubDot n content
    tell $ Subgraph name c
    return name

-- | Cluster
cluster :: Text -> DotGen () -> DotGen GraphName
cluster name = subgraph $ "cluster_" <> name

-- | Cluster, discarding the graph name
cluster_ :: Text -> DotGen () -> DotGen ()
cluster_ name subgraph = void $ cluster name subgraph

-- * Miscelaneous
-- ** Rankdir
rankdir :: RankdirType -> DotGen ()
rankdir = tell . Rankdir

leftRight :: RankdirType
leftRight = LR

topBottom :: RankdirType
topBottom = TB

-- ** Labels
-- | Label declaration for graphs or subgraphs
labelDec :: Text -> DotGen ()
labelDec = tell . Label

-- ** Ports
(.:) :: NodeId
     -> Text -- ^ Port
     -> NodeId
(UserId t) .: p = UserId $ t <> ":" <> p
(Nameless i) .: p = UserId $ T.pack (show i) <> ":" <> p
