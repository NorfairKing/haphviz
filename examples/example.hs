{-# LANGUAGE OverloadedStrings #-}
import           Text.Dot

import qualified Data.Text.IO as T

main :: IO ()
main = T.putStrLn $ renderGraph g

g :: DotGraph
g = graph directed "example" $ do
    n1 <- node "hi"
    n2 <- node "I"
    n3 <- namelessNode []
    n1 --> n2
    n2 --> n3
