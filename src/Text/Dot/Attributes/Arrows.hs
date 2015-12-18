{-# LANGUAGE OverloadedStrings #-}
-- | Attribute names
module Text.Dot.Attributes.Arrows where

import           Text.Dot.Types.Internal

-- * Arrow attributes

arrowhead :: AttributeName
arrowhead = "arrowhead"

arrowtail :: AttributeName
arrowtail = "arrowtail"

-- * Arrow attribute values

box :: AttributeValue
box = "box"

circle :: AttributeValue
circle = "circle"

crow :: AttributeValue
crow = "crow"

diamond :: AttributeValue
diamond = "diamond"

dot :: AttributeValue
dot = "dot"

ediamond :: AttributeValue
ediamond = "ediamond"

empty :: AttributeValue
empty = "empty"

halfopen :: AttributeValue
halfopen = "halfopen"

inv :: AttributeValue
inv = "inv"

invdot :: AttributeValue
invdot = "invdot"

invempty :: AttributeValue
invempty = "invempty"

invodot :: AttributeValue
invodot = "invodot"

none :: AttributeValue
none = "none"

normal :: AttributeValue
normal = "normal"

obox :: AttributeValue
obox = "obox"

odiamond :: AttributeValue
odiamond = "odiamond"

odot :: AttributeValue
odot = "odot"

open :: AttributeValue
open = "open"

tee :: AttributeValue
tee = "tee"

vee :: AttributeValue
vee = "vee"

