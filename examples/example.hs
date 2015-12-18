{-# LANGUAGE OverloadedStrings #-}

import           Text.Dot

main :: IO ()
main = renderToStdOut $ graph directed "example" $ do
        a <- node "a"
        b <- node "b"
        a --> b
        b --> a
        b --> b



