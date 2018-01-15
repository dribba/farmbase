module FileOperations where

import Prelude
import Math
import Data.Foldable
import Control.Bind ((>>=))
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Data.Array (concatMap, filter, foldl, null, (:))
import Data.Array.Partial (head, tail)
import Data.BooleanAlgebra (not)
import Data.Function ((<<<))
import Data.Int (toNumber)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Path (Path, isDirectory, ls, size, root)
import Partial.Unsafe (unsafePartial)

allFiles :: Path -> Array Path
allFiles root = root : concatMap allFiles (ls root)

allFiles' :: Path -> Array Path
allFiles' file = file : do
  child <- ls file
  allFiles' child

square :: Number -> Number
square n = n * n

even :: Int -> Boolean
even n = n `mod` 2 == 0

negative :: Int -> Boolean
negative n = n < 0

onlyFiles file = filter (not <<< isDirectory) list <> concatMap onlyFiles (filter isDirectory list)
  where list = (ls file)

largestFile = foldl (\a b -> if (vSize) > vSize b then a else Just b) Nothing (onlyFiles root)
  where vSize = fromMaybe 0


-- count :: Array Int -> Int
-- count xs =
--   if null xs then 0
--   else (if even x then 1 else 0) + count (unsafePartial tail xs)
--   where
--     x = unsafePartial head xs


squareAll :: Array Number -> Array Number
squareAll = map square

nonNegativeAll :: Array Int -> Array Int
nonNegativeAll = filter (not $ negative)


testSquareAll :: String
testSquareAll = "squareAll = " <> (show $ ((==) [4.0, 9.0]) $ squareAll [2.0, 3.0])

testNonNegativeAll :: String
testNonNegativeAll = "nonNegativeAll = " <> (show  $ ((==) [2, 3]) $ nonNegativeAll [-1, 2, 3, -5])

testAllTrue1 = "all = " <> (show  $ ((==) true) $ all [true, true, true])
testAllTrue2 = "all = " <> (show  $ ((==) false) $ all [true, false, true])

testTesty1 = "testy = " <> (show  $ ((==) true) $ testy [false])
testTesty2 = "testy = " <> (show  $ ((==) true) $ testy [true, false, true])
testTesty3 = "testy = " <> (show  $ ((==) false) $ testy [false, true, false])
testTesty4 = "testy = " <> (show  $ ((==) true) $ testy [true, true, true, false, true])


count :: forall a. (a -> Boolean) -> Array a -> Int
count p = count' 0
  where
    count' acc [] = acc
    count' acc xs = count' (if p $ unsafePartial $ head xs then (acc + 1) else acc) (unsafePartial $ tail xs)

testy = foldl (==) false

reverse = foldl (flip (:)) []

all = foldl (&&) true

testAll =
  log $ join "\n" ([testSquareAll, testNonNegativeAll, testAllTrue1, testAllTrue2, testTesty1, testTesty2, testTesty3, testTesty4])


mkString :: forall s a. (Foldable s, Show a) => String -> String -> String -> s a -> String
mkString init separator end xs = (flip (<>) end) $ foldl (\acc next -> (if acc == "" then init else acc <> separator) <> (show next)) "" xs

join separator xs = mkString "" separator "" xs
