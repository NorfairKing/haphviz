{-# LANGUAGE OverloadedStrings #-}
import           Text.Dot

import qualified Data.Text.IO                       as T
import           Data.Text.Lazy                     (toStrict)
import           Text.Blaze                         (customAttribute, (!))
import           Text.Blaze.Html.Renderer.Text      (renderHtml)
import           Text.Blaze.Html4.Strict            (table, td, tr)
import           Text.Blaze.Html4.Strict.Attributes (border, cellpadding,
                                                     cellspacing)

port = customAttribute "port"
cellborder = customAttribute "cellborder"

main :: IO ()
main = T.putStrLn $ renderGraph g

g :: DotGraph
g = graph directed "example" $ do
    graphDec [compound =: true]
    rankdir leftRight
    nodeDec [width =: "0", height =: "0"]

    x <- newNode
    y <- newNode
    cluster "store" $ do
        nodeDec [shape =: none]
        labelDec "Store"

        node_ x "x"
        node_ y "y"
        return ()

    xh <- newNode
    cluster "heap" $ do
        nodeDec [shape =: none]
        labelDec "Heap"
        node_ xh $ toStrict $ renderHtml $ do
            table ! border "0" ! cellborder "1" ! cellspacing "0" ! cellpadding "5" $ do
                tr $ do
                    td ! port "1" $ "3"
                    td ! port "2" $ "4"
    x --> (xh .: "1")
    y --> (xh .: "2")



