{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Data.Conduit
import qualified Data.Conduit.Combinators as C
import qualified Data.Conduit.Text as CT
import qualified Data.Text as T
import qualified Data.Text.Read as TR
import System.Environment (getArgs)
import Control.Monad.IO.Class()
import Control.Monad.Trans.Resource (ResourceT)
import System.IO.Unsafe (unsafePerformIO)

data Operation = Add | Subtract | Multiply | Divide
    deriving Show

type Result = Either String Double

applyOperation :: Operation -> Double -> Double -> Result
applyOperation op x y = case op of
    Add      -> pure $ x + y
    Subtract -> pure $ x - y
    Multiply -> pure $ x * y
    Divide   -> if y == 0
                then Left "Division by zero"
                else pure $ x / y

operations :: [Operation]
operations = cycle [Add, Subtract, Multiply, Divide]

parseNumber :: MonadFail m => T.Text -> m Double
parseNumber txt = case TR.double txt of
    Right (num, _) -> pure num
    Left err       -> fail $ "Failed to parse number: " ++ T.unpack txt ++ " Error: " ++ err

processNumbers :: ConduitT Double Void (ResourceT IO) Result
processNumbers = do
    mFirstNum <- await
    case mFirstNum of
        Nothing       -> pure $ Left "No numbers in input file."
        Just firstNum -> processRest firstNum operations

processRest :: Double -> [Operation] -> ConduitT Double Void (ResourceT IO) Result
processRest _ [] = pure $ Left "Ran out of operations."
processRest acc ops = do
    mNextNum <- await
    case mNextNum of
        Nothing  -> pure $ Right acc
        Just num -> do
            let (op:ops') = ops
            case applyOperation op acc num of
                e@(Left _) -> pure e
                Right acc' -> processRest acc' ops'

main :: IO ()
main = do
    args <- getArgs
    case args of
        [_, inputFile, outputFile] -> processFiles inputFile outputFile
        _                          -> putStrLn "Usage: program inputFile outputFile"

popLast :: [a] -> ([a], Maybe a)
popLast []     = ([], Nothing)
popLast (x:[]) = ([], Just x)
popLast (x:xs) = ((x:xs'), l)
    where
        (xs', l) = popLast xs

-- first arg is a leftover from the previous stream
parseWords :: Monad m => Maybe T.Text -> ConduitT T.Text T.Text m ()
parseWords (Just l) = do
    str <- await
    case str of
        Just s -> do
            let (ws, l') = popLast $ T.words $ l <> s
            C.yieldMany ws
            parseWords l'
        Nothing -> do
            yield l
            pure ()
parseWords Nothing = do
    str <- await
    case str of
        Just s -> do
            let (ws, l') = popLast $ T.words s
            C.yieldMany ws
            parseWords l'
        Nothing -> pure ()

processFiles :: FilePath -> FilePath -> IO ()
processFiles inputFile outputFile = do
    result <- runConduitRes $
        C.sourceFile inputFile
        .| CT.decodeUtf8
        -- .| C.concatMap T.words  -- might have problems in edge cases, so next function solves them
        .| parseWords Nothing
        .| C.mapM parseNumber
        .| processNumbers
    case result of
        Left err -> putStrLn $ "[ERROR] " ++ err
        Right n  -> do
            writeFile outputFile (show n)
            putStrLn "Done."


