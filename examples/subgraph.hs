{-# LANGUAGE OverloadedStrings #-}

import           Text.Dot

main :: IO ()
main = renderToStdOut $ graph directed "example" $ do
        a <- node "a"
        b <- node "b"
        subgraph "a" $ do
            a --> b
            b --> a
        subgraph "b" $ do
            b --> b



