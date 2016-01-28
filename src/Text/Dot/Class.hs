{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
module Text.Dot.Class where

import           Text.Dot.Gen

import           Control.Monad (void)

import qualified Data.Text     as T

-- | A class of datatypes representable as a graph with a given config.
class Graph g c | c -> g, g -> c where
    -- | The default config for rendering a @g@.
    defaultGenConfig :: c

    -- | Draw @g@ using @c@ to configure its rendering.
    genGraph :: c -> g -> DotGen ()


-- | Render a @g@ using the default render config
genDefault :: Graph g c => g -> DotGen ()
genDefault = genGraph defaultGenConfig

