{-# LANGUAGE OverloadedStrings #-}

import           Control.Monad (forM, mapM_)
import qualified Data.Text     as T
import           Text.Dot

main :: IO ()
main = renderToStdOut $ graph_ undirected $ do
        ns <- forM [1..n] $ node . T.pack . show
        sequence_ $ concatMap (\(a, as) -> [a --> b | b <- as]) $ zip ns $ tails ns
  where
    n = 6

tails :: [a] -> [[a]]
tails [] = [[]]
tails (a:as) = as : tails as
