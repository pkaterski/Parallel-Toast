{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveFunctor #-}


module Main1 where

import Control.Monad.Free
import Control.Monad.Trans.State
import Control.Monad.Trans.Except
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Except (throwError)
import Lens.Micro
import Lens.Micro (over)
import Lens.Micro.Mtl
import System.Environment (getArgs)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Text.Read (double)
import Control.Monad.Trans.Class

-- Define the operations as a functor for the free monad
data OpF next
    = Add Double next
    | Subtract Double next
    | Multiply Double next
    | Divide Double next
    deriving Functor

type OpFree = Free OpF

-- Lift operations into the free monad
add :: Double -> OpFree ()
add x = liftF (Add x ())

sub :: Double -> OpFree ()
sub x = liftF (Subtract x ())

multiply :: Double -> OpFree ()
multiply x = liftF (Multiply x ())

divide :: Double -> OpFree ()
divide x = liftF (Divide x ())

-- Data structure for calculator state with a lens
data CalcState = CalcState
    { _result :: !Double  -- Using bang pattern for strictness
    } deriving Show

-- Define a lens for the result field
result :: Lens' CalcState Double
result f s = fmap (\x -> s { _result = x }) (f (_result s))

-- Monad stack: StateT for state, ExceptT for error handling
type CalcM = ExceptT String (State CalcState)

-- Interpreter for the free monad using monad transformers and microlens
interpret :: OpFree () -> CalcM ()
interpret (Free (Add x next)) = do
    calc <- lift $ get
    lift $ put $ over result (+ x) calc
    -- result %= (+ x)
    interpret next
interpret (Free (Subtract x next)) = do
    result %= subtract x
    interpret next
interpret (Free (Multiply x next)) = do
    result %= (* x)
    interpret next
interpret (Free (Divide x next)) = do
    if x == 0
        then throwError "Division by zero"
        else result %= (/ x)
    interpret next
interpret (Pure _) = return ()

-- Build the computation from the list of numbers
buildOps :: [Double] -> OpFree ()
buildOps xs = buildOps' xs 0
  where
    ops = [add, sub, multiply, divide]
    buildOps' [] _ = return ()
    buildOps' (y:ys) !idx = do  -- Bang pattern for strict index evaluation
        let op = ops !! (idx `mod` 4)
        op y
        buildOps' ys (idx + 1)

-- Main function to read input, process operations, and write output
main :: IO ()
main = do
    args <- getArgs
    mapM_ putStrLn args
    [_, inputFile, outputFile] <- getArgs
    content <- TIO.readFile inputFile
    let lines = T.lines content
        numbers = map parseDouble lines
    case sequence numbers of
        Left err -> putStrLn ("Error parsing input file: " ++ err)
        Right nums@(x:xs) -> do
            let initialState = CalcState x
                computation = buildOps xs
                (resultState, finalState) = runState (runExceptT (interpret computation)) initialState
            case resultState of
                Left err -> putStrLn ("Calculation error: " ++ err)
                Right _ -> writeFile outputFile (show (_result finalState))
        Right [] -> putStrLn "No numbers in input file."

-- Parse a line into a Double
parseDouble :: T.Text -> Either String Double
parseDouble t = case double t of
    Right (d, _) -> Right d
    Left err     -> Left err

-- main :: IO ()
-- main = putStrLn "Pesho"
