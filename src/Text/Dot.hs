{-# LANGUAGE OverloadedStrings #-}
module Text.Dot (
      module Text.Dot.Gen
    -- * Graph rendering
    , renderGraph
    , renderToFile
    , renderToStdOut
    ) where

import           Text.Dot.Gen
import           Text.Dot.Render (renderGraph, renderToFile, renderToStdOut)

