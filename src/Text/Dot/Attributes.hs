{-# LANGUAGE OverloadedStrings #-}
module Text.Dot.Attributes (
      module Text.Dot.Attributes
    -- * Arrows
    , module Text.Dot.Attributes.Arrows
    ) where

import           Text.Dot.Attributes.Arrows
import           Text.Dot.Types.Internal

-- * Attribute Names

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

-- * Attribute values

true :: AttributeValue
true = "true"

false :: AttributeValue
false = "false"
