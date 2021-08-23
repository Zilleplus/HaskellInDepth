import Data.Foldable
import Control.Monad.State

addItem :: Integer -> State Integer ()
addItem n = do
  s <- get
  put (s + n)

addItem' :: Integer -> State Integer ()
addItem' n = modify (+n)

sumList :: [Integer] -> State Integer ()
sumList = traverse_ addItem
-- traverse_ is needs as the output is nothing
-- traverse_ (Foldable t, Applicative f) => (a -> f b) -> t a -> f ()

answer :: Integer
answer = execState (sumList [1..100]) 0

main :: IO ()
main = print answer
