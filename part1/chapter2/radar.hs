{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}
import Data.Semigroup
import qualified Data.Text as T
import Fmt
import System.Environment

import System.Random.Stateful

class (Eq a, Enum a, Bounded a) => CyclicEnum a where
    cpred :: a -> a
    cpred d
        | d == minBound = maxBound
        | otherwise = pred d
    csucc :: a -> a 
    csucc d 
        | d == maxBound = minBound
        | otherwise = succ d


data Direction = North | East | South | West
    deriving (Eq, Enum, Bounded, Show, Read, Ord)

data Turn = TNone | TLeft | TRight | TAround
    deriving (Eq, Enum, Bounded, Show, Read, Ord)

-- usefull extension:

instance CyclicEnum Direction

instance CyclicEnum Turn

rotate :: Turn -> Direction -> Direction
rotate TNone = id
rotate TLeft = cpred
rotate TRight = csucc
rotate TAround = cpred . cpred

every :: (Enum a, Bounded a) => [a]
every = enumFrom minBound

orient :: Direction -> Direction -> Turn
orient d1 d2 = head $ filter (\t -> rotate t d1 == d2) every

-- TODO:: at page 25
rotateMany :: Direction -> [Turn] -> Direction 
rotateMany = foldl (\ld lt -> rotate lt ld)
--rotateMany  = foldl (flip rotate) -- from book, flip is pretty cool

rotateManySteps :: Direction -> [Turn] -> [Direction]
rotateManySteps  = scanl (flip rotate)
-- scanl is mimilar to foldl, but returns a list of sccuessive reduced values from the left.

orientMany :: [Direction] -> [Turn]
orientMany ds@(_:_:_) = zipWith orient ds (tail ds)
orientMany [] = []

instance Semigroup Turn where
    TNone <> t = t
    TLeft <> TLeft = TAround
    TLeft <> TRight = TNone
    TLeft <> TAround = TRight
    TRight <> TRight = TAround
    TRight <> TAround = TLeft
    TAround <> TAround = TNone
    t1 <> t2 = t2 <> t1

instance Monoid Turn where
    mempty = TNone

rotateMany' :: Direction -> [Turn] -> Direction
rotateMany' dir ts = rotate (mconcat ts) dir -- instead of mconcat we can use fold

-- Without overloaded strings extension we would have to do this:
-- instance Buildable Direction where
--     build North = fromString "N"
--     build East = fromString "E"
--     build South = fromString "S"
--     build West = fromString "W"

instance Buildable Direction where
    build North = "N"
    build East = "E"
    build South = "S"
    build West = "W"

instance Buildable Turn where
    build TNone = "--"
    build TLeft = "<-"
    build TRight = "->"
    build TAround = "||"

orientFromFile :: FilePath -> IO ()
orientFromFile fname = do
    f <- readFile fname
    let dirs = map read $ lines f
        orients = orientMany dirs
    fmtLn $ nameF "All turns" (unwordsF orients)

rotateFromFile :: Direction -> FilePath -> IO ()
rotateFromFile dir fname = do
    f <- readFile fname
    let turns = map read $ lines f 
        finalDir = rotateMany dir turns
        dirs = rotateManySteps dir turns
    fmtLn $ "Final Direction: " +|| finalDir ||+ ""
    fmt $ nameF "Intermediate directoins" (unwordsF dirs)

main :: IO()
main = do 
    args <- getArgs
    case args of 
      ["-r", fname, dir] -> rotateFromFile (read dir) fname
      ["-o", fname] -> orientFromFile fname
      _ -> putStrLn $ "Usuage: locator -o filename \n" ++ 
                      "        locator -r filename direction"
