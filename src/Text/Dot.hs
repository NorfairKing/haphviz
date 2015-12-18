{-# LANGUAGE OverloadedStrings #-}

-- | Graphviz code-generation in Haskell
--
-- This can be used to generate graphviz code for large graph visualisations.
--
-- > {-# LANGUAGE OverloadedStrings #-}
-- > import Text.Dot
-- >
-- > main :: IO ()
-- > main = renderToStdOut $ graph directed "example" $ do
-- >     a <- node "a"
-- >     b <- node "b"
-- >     a --> b
-- >     b --> a
-- >     b --> b
--
-- >>> runhaskell example.hs
-- > digraph example {
-- >   0 [label=<a>];
-- >   1 [label=<b>];
-- >   0 -> 1;
-- >   1 -> 0;
-- >   1 -> 1;
-- > }
--
--
module Text.Dot (
      module Text.Dot.Gen
    -- * Graph rendering
    , renderGraph
    , renderToFile
    , renderToStdOut
    ) where

import           Text.Dot.Gen
import           Text.Dot.Render (renderGraph, renderToFile, renderToStdOut)

