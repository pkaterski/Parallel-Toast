module Main (main) where

import Types
import Config
import Streaming
import Data.Conduit
import qualified Data.Conduit.Combinators as C
import qualified Data.Conduit.Text as CT
import System.Environment (getArgs)
import Control.Monad.IO.Class()
import UnliftIO (mapConcurrently_)
import Data.Time.Clock (getCurrentTime, diffUTCTime)
import Lens.Micro
import Control.Concurrent.STM
import Control.Monad.Reader


main :: IO ()
main = do
    startTime <- getCurrentTime
    confPath <- getConfigPath
    mConf <- readConfigFromFile confPath
    case mConf of
        Just conf -> do
            logVar           <- atomically $ newTVar []
            jobsStartedVar   <- atomically $ newTVar 0
            jobsComplitedVar <- atomically $ newTVar 0
            jobsFailedVar    <- atomically $ newTVar 0

            let env = Env logVar conf jobsStartedVar jobsComplitedVar jobsFailedVar

            runReaderT runApp env
            endTime <- getCurrentTime
            let elapsedTime = diffUTCTime endTime startTime
            putStrLn $ "Elapsed time: " <> show elapsedTime
        Nothing -> putStrLn "Exiting: Error reading config..."

runApp :: AppM ()
runApp = do
    conf <- asks envConfig
    let batches = getBatches conf
    mapM_ runBatch batches

runBatch :: [Job] -> AppM ()
runBatch batch = mapConcurrently_ runJob batch

getConfigPath :: IO String
getConfigPath = do
    args <- getArgs
    case args of
        [_, conf] -> pure conf
        _         -> pure "config.json"

runJob :: Job -> AppM ()
runJob job = do
    let inputFile      = job ^. inFile
        outputFile     = job ^. outFile
        currJobId      = "[" <> show (job ^. jobId) <> "]"
        currOperations = cycle $ job ^. operations
    result <- liftIO $ runConduitRes $
        C.sourceFile inputFile
        .| CT.decodeUtf8
        .| parseWords
        .| parseNumber
        .| processNumbers currOperations
    case result ^. value of
        Nothing -> appendLog $ currJobId <> " Failed: No result produced"
        Just n  -> do
            liftIO $ writeFile outputFile (show n)
            appendLog $ currJobId <> " Done. Saved: " <> outputFile

appendLog :: String -> AppM ()
appendLog msg = do
    logVar <- asks envLogVar
    liftIO $ atomically $ do
        logs <- readTVar logVar
        writeTVar logVar (msg:logs)
    liftIO $ putStrLn msg -- for clarity

