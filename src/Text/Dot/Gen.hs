{-# LANGUAGE OverloadedStrings #-}
-- | Generating graph contents
module Text.Dot.Gen (
      module Text.Dot.Gen

    , Dot
    , DotGraph
    , NodeId
    , Attribute
    , AttributeName
    , AttributeValue
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


-- | Generate a haphviz graph with a given name and content
--
-- >>> graph directed "mygraph" $ do
--         a <- node "a"
--         b <- node "b"
--         a --> b
-- > graph mygraph {
-- >   0 [label="a"];
-- >   1 [label="b"];
-- >   0 -- 1;
-- > }
graph :: GraphType
      -> GraphName -- ^ Internal graph name
      -> DotGen a  -- ^ Content
      -> DotGraph
graph gt gn func = Graph gt gn $ genDot func

-- | Like 'graph' but without an internal graph name
--
-- > graph_ gt func = graph gt "haphviz" func
graph_ :: GraphType
       -> DotGen a -- ^ Content
       -> DotGraph
graph_ gt func = graph gt "haphviz" func

-- | Generate Internal dot content AST
genDot :: DotGen a -> Dot
genDot = genSubDot 0

-- | Utility function to generate a graph with nameless nodes starting from a given starting number.
genSubDot :: Int -> DotGen a -> Dot
genSubDot n func = runIdentity $ execWriterT $ execStateT func n

-- * Graph types

-- | Directed graph
--
-- >>> directed
-- > digraph
directed :: GraphType
directed = DirectedGraph

-- | Undirected graph
--
-- >>> undirected
-- > graph
undirected :: GraphType
undirected = UndirectedGraph

-- * Nodes

-- | Most general node declaration
--
-- This allows you to specify a node identifier for the node.
--
-- In general it is more efficient to use nameless nodes and have the identifiers generated for you.
--
-- It also allows you to specify attributes.
-- In general it is better to use 'namelessNode'.
--
-- >>> n <- newNode
-- >>> genNode n [color =: green]
-- > 0 [color="green"];
genNode :: NodeId -> [Attribute] -> DotGen ()
genNode ni ats = tell $ Node ni ats

-- | Node with given (internal) name and attributes
--
-- Aside from human-readable output, there is no reason to use named nodes.
-- Use 'node' instead.
--
-- >>> void $ namedNode "woohoo" [color =: red]
-- > wohoo [color="red"];
namedNode :: Text -- ^ Name
          -> [Attribute] -> DotGen NodeId
namedNode t ats = do
    let ni = UserId t
    genNode ni ats
    return ni

-- | Nameless node with attributes
--
-- This generates a nameless node for you but still allows you to specify its individual attributes.
-- In general it is better to use 'nodeDec' and then 'node'.
--
-- >>> void $ namelessNode [color =: blue]
-- > 0 [color="blue"];
namelessNode :: [Attribute] -> DotGen NodeId
namelessNode ats = do
    ni <- newNode
    genNode ni ats
    return ni

-- | Node with a label but no other attributes
--
-- A node with a given label and no other attributes.
-- Usually used in conjunction with 'nodeDec'.
--
-- >>> void $ node "server"
-- > 0 [label="server"];
node :: Text -- ^ Label
     -> DotGen NodeId
node l = namelessNode [label =: l]

-- | Node with given node Id and label
--
-- > node_ ni l = genNode ni [label =: l]
node_ :: NodeId -- ^ given Node ID
      -> Text -- ^ Label
      -> DotGen ()
node_ ni l = genNode ni [label =: l]

-- | Generate a new internally nameless node ID
--
-- It is not generally a good idea to use this directly but it can be used to define node identifiers before a subgraph to reference them both in- and outside of it.
newNode :: DotGen NodeId
newNode = do
    i <- get
    modify (+1)
    return $ Nameless i


-- * Edges

-- | Most general edge declaration
--
-- This allows you to specify attributes for a single edge.
--
-- Usually it is better to use 'edgeDec' and then '-->'.
--
-- >>> genEdge a b [label =: "MyEdge"]
-- > a -> b [label="MyEdge"];
genEdge :: NodeId -> NodeId -> [Attribute] -> DotGen ()
genEdge n1 n2 ats = tell $ Edge n1 n2 ats


-- | Infix edge constructor. (No attributes)
--
-- This takes care of using the right edge declaration for the given graph.
--
-- For undirected graphs, the output would be @--@ ...
--
-- >>> a --> b
-- > a -- b;
--
-- ... and for directed graphs it would be @->@.
--
-- >>> a --> b
-- > a -> b;
(-->) :: NodeId -> NodeId -> DotGen ()
n1 --> n2 = genEdge n1 n2 []

-- * Attributes

-- | Infix operator for an attribute pair
--
-- >>> [label =: "MyNode"]
-- > [label="MyNode"]
(=:) :: AttributeName -> AttributeValue -> Attribute
(=:) = (,)

-- ** Attribute Names

-- |
-- >>> label
-- > label
label :: AttributeName
label = "label"

-- |
-- >>> compound
-- > compound
compound :: AttributeName
compound = "compound"

-- |
-- >>> shape
-- > shape
shape :: AttributeName
shape = "shape"

-- |
-- >>> color
-- > color
color :: AttributeName
color = "color"

-- |
-- >>> dir
-- > dir
dir :: AttributeName
dir = "dir"

-- |
-- >>> width
-- > width
width :: AttributeName
width = "width"

-- |
-- >>> height
-- > height
height :: AttributeName
height = "height"

-- ** Attribute values

-- |
-- >>> true
-- > true
true :: AttributeValue
true = "true"

-- |
-- >>> true
-- > true
false :: AttributeValue
false = "false"

-- |
-- >>> none
-- > none
none :: AttributeValue
none = "none"


-- * Declarations

-- | General declaration of common attributes
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


-- | Cluster with a given name
--
-- The @cluster_@ prefix is taken care of.
cluster :: Text -> DotGen () -> DotGen GraphName
cluster name = subgraph $ "cluster_" <> name

-- | Like 'cluster', discarding the graph name.
cluster_ :: Text -> DotGen () -> DotGen ()
cluster_ name subgraph = void $ cluster name subgraph

-- | Subgraph declaration
--
-- This is rarely useful. Just use 'cluster'.
subgraph :: Text -> DotGen () -> DotGen GraphName
subgraph name content = do
    n <- get
    let c = genSubDot n content
    tell $ Subgraph name c
    return name

-- * Miscelaneous
-- ** Rankdir

-- | The rankdir declaration
--
--  This changes the default layout of nodes
--
-- >>> rankdir leftRight
-- > rankdir = LR;
rankdir :: RankdirType -> DotGen ()
rankdir = tell . Rankdir

-- | > LR
leftRight :: RankdirType
leftRight = LR

-- | > TB
topBottom :: RankdirType
topBottom = TB

-- ** Labels
-- | Label declaration for graphs or subgraphs
labelDec :: Text -> DotGen ()
labelDec = tell . Label

-- ** Ports

-- | Use a certain port on a given node's label as an endpoint for an edge
(.:) :: NodeId
     -> Text -- ^ Port
     -> NodeId
(UserId t) .: p = UserId $ t <> ":" <> p
(Nameless i) .: p = UserId $ T.pack (show i) <> ":" <> p

-- * Internals
-- | Generation monad
type DotGen = StateT State (WriterT Dot Identity)

-- | The next id for a nameless node
type State = Int

