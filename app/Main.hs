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
--import qualified Control.Concurrent as Concurrent
import Control.Concurrent.Async
import Data.Time.Clock (getCurrentTime, diffUTCTime)
import Control.Exception


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
operations = cycle [Add, Subtract]
--operations = cycle [Add, Subtract, Multiply, Divide]

parseNumber :: MonadFail m => T.Text -> m Double
parseNumber txt = case TR.double txt of
    Right (num, "") -> pure num
    Right (_, l)    -> fail $ "Failed to parse number: " ++ T.unpack txt ++ " Error: Leftover: " <> T.unpack l
    Left err        -> fail $ "Failed to parse number: " ++ T.unpack txt ++ " Error: " ++ err

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

tasks :: [IO ()]
tasks =
    fmap ((\i -> processFile ("i" ++ i ++ ".txt") ("o" ++ i ++ ".txt")) . show) ([1..50] :: [Int])

chunk :: Int -> [a] -> [[a]]
chunk _ [] = []
chunk n xs = take n xs : chunk n (drop n xs)

main :: IO ()
main = do
    startTime <- getCurrentTime
    let batches = chunk 4 tasks

    mapM_ (\batch -> mapConcurrently_ (\p -> handle onErr p) batch) batches
    --mapM_ id tasks
    --
    endTime <- getCurrentTime
    let elapsedTime = diffUTCTime endTime startTime
    putStrLn $ "Elapsed time: " ++ show elapsedTime
    -- args <- getArgs
    -- case args of
    --     [_, inputFile, outputFile] -> processFile inputFile outputFile
    --     _                          -> putStrLn "Usage: program inputFile outputFile"

popLast :: [a] -> ([a], Maybe a)
popLast []     = ([], Nothing)
popLast (x:[]) = ([], Just x)
popLast (x:xs) = ((x:xs'), l)
    where
        (xs', l) = popLast xs

-- this parsing handles leftover from the previous stream
parseWords :: Monad m => ConduitT T.Text T.Text m ()
parseWords = go Nothing
  where
    go leftover = do
        str <- await
        case str of
            Nothing -> yieldLeftover leftover
            Just s  -> handleNextChunck leftover s

    mergeLeftover Nothing s  = s
    mergeLeftover (Just l) s = l <> s

    yieldLeftover Nothing  = pure ()
    yieldLeftover (Just l) = yield l

    handleNextChunck leftover s = do
        let (ws, l') = popLast $ T.words $ mergeLeftover leftover s
        C.yieldMany ws
        if T.last s == '\n'
            then do
                yieldLeftover l'
                go Nothing
            else go l'

processFile' :: FilePath -> FilePath -> IO ()
processFile' fi fo = handle onErr $ runProcess fi fo
    where
        onErr :: SomeException -> IO ()
        onErr e = do
            putStrLn $ "action was killed by: " ++ displayException e
        runProcess :: FilePath -> FilePath -> IO ()
        runProcess inputFile outputFile = do
            result <- runConduitRes $
                C.sourceFile inputFile
                .| CT.decodeUtf8
                -- .| C.concatMap T.words  -- simpler but has a bug
                .| parseWords
                .| C.mapM parseNumber
                .| processNumbers
            case result of
                Left err -> putStrLn $ "[ERROR] " ++ err
                Right n  -> do
                    writeFile outputFile (show n)
                    putStrLn $ "Done: " <> outputFile

onErr :: SomeException -> IO ()
onErr e = do
    putStrLn $ "action was killed by: " ++ displayException e

processFile :: FilePath -> FilePath -> IO ()
processFile inputFile outputFile = do
    result <- runConduitRes $
        C.sourceFile inputFile
        .| CT.decodeUtf8
        -- .| C.concatMap T.words  -- simpler but has a bug
        .| parseWords
        .| C.mapM parseNumber
        .| processNumbers
    case result of
        Left err -> putStrLn $ "[ERROR] " ++ err
        Right n  -> do
            writeFile outputFile (show n)
            putStrLn $ "Done: " <> outputFile


