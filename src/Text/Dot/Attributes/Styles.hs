{-# LANGUAGE OverloadedStrings #-}

-- | Style attributes.
--
-- See http://www.graphviz.org/doc/info/attrs.html#k:style for more info on graphviz styles
module Text.Dot.Attributes.Styles where

import           Text.Dot.Types.Internal

-- * Style attributes

style :: AttributeName
style = "style"

-- * Style attribute values

bold :: AttributeValue
bold = "bold"

striped :: AttributeValue
striped = "striped"

filled :: AttributeValue
filled = "filled"

solid :: AttributeValue
solid = "solid"

dashed :: AttributeValue
dashed = "dashed"

dotted :: AttributeValue
dotted = "dotted"

rounded :: AttributeValue
rounded = "rounded"

-- ** Styles only for nodes

diagonals :: AttributeValue
diagonals = "diagonals"

wedged :: AttributeValue
wedged = "wedged"

