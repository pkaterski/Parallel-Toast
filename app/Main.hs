module Main (main) where

import Types
import Config
import Streaming
import Data.Conduit
import qualified Data.Conduit.Combinators as C
import qualified Data.Conduit.Text as CT
import System.Environment (getArgs)
import Control.Monad.IO.Class()
import UnliftIO (mapConcurrently_, SomeException, displayException)
import Data.Time.Clock (getCurrentTime, diffUTCTime)
import Lens.Micro
import Control.Concurrent.STM
import Control.Monad.Reader
import Data.List (intercalate)
import Control.Monad.Trans.Resource (ResourceT)


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

            let env = Env logVar conf jobsStartedVar jobsComplitedVar

            runReaderT runApp env
            endTime <- getCurrentTime
            let elapsedTime = diffUTCTime endTime startTime
            putStrLn $ "Elapsed time: " <> show elapsedTime
            putStrLn "Saving log..."
            logs <- readTVarIO logVar
            writeFile (conf ^. logFile) $ intercalate "\n" logs
            putStrLn "Done."
            jobsBegin <- readTVarIO jobsStartedVar
            jobsEnd   <- readTVarIO jobsComplitedVar
            putStrLn $ "Jobs Started: " <> show jobsBegin
            putStrLn $ "Jobs Completed: " <> show jobsEnd

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
        [conf] -> pure conf
        _      -> pure "config.json"

handleErrs :: TVar Log -> String -> SomeException -> ConduitT i o (ResourceT IO) ()
handleErrs logVar jobStr e = do
    let logStr = "[ERROR] Job with ID " <> jobStr <> "was killed by: " <> displayException e
    liftIO $ writeToLog logVar logStr

runJob :: Job -> AppM ()
runJob job = do
    jobsStartedVar <- asks envJobsStarted
    liftIO $ atomically $ modifyTVar' jobsStartedVar (+1)

    let inputFile      = job ^. inFile
        outputFile     = job ^. outFile
        currJobId      = "[" <> show (job ^. jobId) <> "]"
        currOperations = cycle $ job ^. operations

    -- for error handling
    logVar <- asks envLogVar
    let customHandle = handleErrs logVar currJobId

    result <- liftIO $ runConduitRes $
         (handleC customHandle $ C.sourceFile inputFile .| CT.decodeUtf8)
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
            jobsEndedVar <- asks envJobsCompleted
            liftIO $ atomically $ modifyTVar' jobsEndedVar (+1)


appendLog :: String -> AppM ()
appendLog msg = do
    logVar <- asks envLogVar
    liftIO $ writeToLog logVar msg

writeToLog :: TVar Log -> String -> IO ()
writeToLog logVar msg = do
    atomically $ do
        logs <- readTVar logVar
        writeTVar logVar (msg:logs)
    putStrLn msg -- for clarity

