{-# LANGUAGE OverloadedStrings #-}

import           Data.List          (unfoldr)
import           Data.Maybe         (listToMaybe)

import           Control.Monad      (forM, sequence_)
import           System.Environment (getArgs)

import qualified Data.Text          as T
import           Text.Dot

main :: IO ()
main = do
    args <- getArgs
    let n = case args of
                [] -> 50
                [ns] -> read ns
                _ -> error "Usage: runhaskell divisors.hs INT"

    renderToStdOut $ graph directed "divisors" $ do
        nodes <- forM [1..n] $ node . T.pack . show
        let ns = zip [1..] nodes
        sequence_ [ni --> nj | (i, ni) <- ns, (j, nj) <- ns, i > j, primary i j]

-- Below here is math, ignore!

primary :: Integer -> Integer -> Bool
primary i 1 = prime i
primary i j = i `rem` j == 0 && prim
  where prim = null . filter (\k -> i `rem` k == 0) . takeWhile (< i) . map (\k -> j * k) $ [2..]

prime :: Integer -> Bool
prime n = n > 1 &&
              foldr (\p r -> p*p > n || ((n `rem` p) /= 0 && r))
                True primes

primes :: [Integer]
primes = 2 : 3 : [x | x <- [5,7..], head (pfactors (tail primes) x) == x]
  where
    pfactors prs n = unfoldr (\(ds,n) -> listToMaybe
        [(x, (dropWhile (< x) ds, div n x)) | x <- takeWhile ((<=n).(^2)) ds ++
                                                    [n|n>1], mod n x==0]) (prs,n)
