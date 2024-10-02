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
import Data.List (intercalate)


main :: IO ()
main = do
    startTime <- getCurrentTime
    confPath  <- getConfigPath
    mConf     <- readConfigFromFile confPath
    case mConf of
        Just conf -> do
            logVar           <- newTVarIO []
            jobsStartedVar   <- newTVarIO 0
            jobsComplitedVar <- newTVarIO 0
            jobsFailedVar    <- newTVarIO 0

            let env = Env logVar conf jobsStartedVar jobsComplitedVar jobsFailedVar

            runReaderT runApp env
            endTime <- getCurrentTime
            let elapsedTime = diffUTCTime endTime startTime
            putStrLn $ "Elapsed time: " <> show elapsedTime
            putStrLn "Saving log..."
            logs <- readTVarIO logVar
            writeFile (conf ^. logFile) $ intercalate "\n" logs

        Nothing -> putStrLn "Exiting: Error reading config..."

runApp :: AppM ()
runApp = do
    conf <- asks envConfig
    let batches = getBatches conf
    mapM_ runBatch batches

runBatch :: [Job] -> AppM ()
runBatch = mapConcurrently_ runJob

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

    let errStr  = "[ERROR] " <> currJobId <> " "
    let infoStr = "[INFO] " <> currJobId <> " "
    mapM_ (appendLog . (errStr <>)) (result ^. errors)

    case result ^. value of
        Nothing -> appendLog $ errStr <> "Failed: No result produced"
        Just n  -> do
            liftIO $ writeFile outputFile (show n)
            appendLog $ infoStr <> "Done. Saved: " <> outputFile

appendLog :: String -> AppM ()
appendLog msg = do
    logVar <- asks envLogVar
    liftIO $ atomically $ do
        logs <- readTVar logVar
        writeTVar logVar (msg:logs)
    liftIO $ putStrLn msg -- for clarity

