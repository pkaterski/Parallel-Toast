module Helper
    ( chunk
    , popLast
    , applyOperation
    , enqueue
    ) where

import Types
import Control.Concurrent.STM

-- | Splits up a list into chunks of size n
chunk :: Int -> [a] -> [[a]]
chunk _ [] = []
chunk n xs = take n xs : chunk n (drop n xs)

-- | Pops the last element of a list
popLast :: [a] -> ([a], Maybe a)
popLast []     = ([], Nothing)
popLast [x]    = ([], Just x)
popLast (x:xs) = (x:xs', l)
  where
    (xs', l) = popLast xs

-- | Apply operation to two numbers
applyOperation :: Operation -> Double -> Double -> NumberOrErr
applyOperation op x y = case op of
    Add      -> pure $ x + y
    Subtract -> pure $ x - y
    Multiply -> pure $ x * y
    Divide   -> if y == 0
                then Left "Division by zero"
                else pure $ x / y

enqueue :: TQueue a -> [a] -> STM () 
enqueue _ []     = pure ()
enqueue q (x:xs) = do
    writeTQueue q x
    enqueue q xs
