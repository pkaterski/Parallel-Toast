{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Types
import Config
import Streaming
import Data.Conduit
import qualified Data.Conduit.Combinators as C
import qualified Data.Conduit.Text as CT
import System.Environment (getArgs)
import Control.Monad.IO.Class()
import Control.Concurrent.Async
import Data.Time.Clock (getCurrentTime, diffUTCTime)
import Control.Exception
import Lens.Micro


main :: IO ()
main = do
    startTime <- getCurrentTime
    mConf <- readConfigFromFile "config.json"
    case mConf of
        Just conf -> do
            let batches = getBatches conf

            mapM_ (\batch -> mapConcurrently_ handleJob batch) batches
            endTime <- getCurrentTime
            let elapsedTime = diffUTCTime endTime startTime
            putStrLn $ "Elapsed time: " ++ show elapsedTime
        Nothing -> putStrLn "Exiting: Error reading config..."

handleJob :: Job -> IO ()
handleJob job = let currJobId = job ^. jobId in
    handle (onErr currJobId) $ runJob job

onErr :: Int -> SomeException -> IO ()
onErr currJobId e = do
    let jobStr = "[" <> show currJobId <> "]"
    putStrLn $ "Job with ID " <> jobStr <> "was killed by: " <> displayException e

runJob :: Job -> IO ()
runJob job = do
    let inputFile      = job ^. inFile
        outputFile     = job ^. outFile
        currJobId      = "[" <> show (job ^. jobId) <> "]"
        currOperations = cycle $ job ^. operations
    result <- runConduitRes $
        C.sourceFile inputFile
        .| CT.decodeUtf8
        .| parseWords
        .| parseNumber
        .| processNumbers currOperations
    case result ^. value of
        Nothing -> pure ()
        Just n  -> do
            writeFile outputFile (show n)
            putStrLn $ currJobId <> " Done. Saved: " <> outputFile


