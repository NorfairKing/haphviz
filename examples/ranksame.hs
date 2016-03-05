{-# LANGUAGE OverloadedStrings #-}

import           Text.Dot

main :: IO ()
main = renderToStdOut $ graph directed "example" $ do
        a <- newNode
        b <- newNode
        ranksame $ do
            node_ a "a"
            node_ c "c"
        b <- node "b"
        a --> b
        c --> b



