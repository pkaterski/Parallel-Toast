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
import Lens.Micro


data Operation = Add | Subtract | Multiply | Divide
    deriving Show

type NumberOrErr = Either String Double

data Result = Result
    { _value  :: Maybe Double
    , _errors :: [String]
    } deriving Show

value :: Lens' Result (Maybe Double)
value = lens _value (\s v -> s { _value = v })

errors :: Lens' Result [String]
errors = lens _errors (\s e -> s { _errors = e })

applyOperation :: Operation -> Double -> Double -> NumberOrErr
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

parseNumber :: Monad m => ConduitT T.Text NumberOrErr m ()
parseNumber = do
    mTxt <- await
    case mTxt of
        Just txt -> case TR.double txt of
            Right (num, "") -> do
                yield $ Right num
                -- continue the processing
                parseNumber
            Right (num, l) -> do
                yield $ Right num
                yield $ Left $ "Failed to parse number: " <> T.unpack txt <> " Error: Leftover: " <> T.unpack l
                -- stop the processing
                pure ()
            Left err -> do
                yield $ Left $ "Failed to parse number: " <> T.unpack txt <> " Error: " <> err
                -- stop the processing
                pure ()
        Nothing -> pure ()

processNumbers :: ConduitT NumberOrErr Void (ResourceT IO) Result
processNumbers = do
    mFirstNum <- await
    case mFirstNum of
        Nothing          -> pure $ Result Nothing ["No numbers in input file."]
        Just (Right num) -> processRest (Result (Just num) []) operations
        Just (Left err)  -> processRest (Result Nothing [err]) operations

processRest :: Result -> [Operation] -> ConduitT NumberOrErr Void (ResourceT IO) Result
processRest acc [] = pure acc
processRest acc ops = do
    mNextNum <- await
    case mNextNum of
        Nothing  -> pure acc
        Just (Right num) -> do
            case (acc ^. value) of
                Just curr -> do
                    let (op:ops') = ops
                    case applyOperation op curr num of
                        Left e    -> pure $ acc & errors %~ (e:)
                        Right val -> processRest (acc & value .~ Just val ) ops'
                Nothing -> processRest (acc & value .~ Just num) ops
        Just (Left err) -> processRest (acc & errors %~ (err:)) ops

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
    go leftoverStr = do
        str <- await
        case str of
            Nothing -> yieldLeftover leftoverStr
            Just s  -> handleNextChunck leftoverStr s

    mergeLeftover Nothing s  = s
    mergeLeftover (Just l) s = l <> s

    yieldLeftover Nothing  = pure ()
    yieldLeftover (Just l) = yield l

    handleNextChunck leftoverStr s = do
        let (ws, l') = popLast $ T.words $ mergeLeftover leftoverStr s
        C.yieldMany ws
        if T.last s == '\n'
            then do
                yieldLeftover l'
                go Nothing
            else go l'

onErr :: SomeException -> IO ()
onErr e = do
    putStrLn $ "action was killed by: " ++ displayException e

processFile :: FilePath -> FilePath -> IO ()
processFile inputFile outputFile = do
    result <- runConduitRes $
        C.sourceFile inputFile
        .| CT.decodeUtf8
        .| parseWords
        .| parseNumber
        .| processNumbers
    case result ^. value of
        Nothing -> pure ()
        Just n  -> do
            writeFile outputFile (show n)
            putStrLn $ "Done. Saved:" <> outputFile


