{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}

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
import Lens.Micro.TH
import qualified Data.Aeson as A
import GHC.Generics
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Lazy.Char8 as BL8


data Operation = Add | Subtract | Multiply | Divide
    deriving (Show, Generic)

instance A.FromJSON Operation
instance A.ToJSON Operation

type NumberOrErr = Either String Double

data Result = Result
    { _value  :: Maybe Double
    , _errors :: [String]
    } deriving Show

makeLenses ''Result

customOptions :: A.Options
customOptions = A.defaultOptions
    { A.fieldLabelModifier = drop 1
    }

data Job = Job
    { _jobId      :: Int
    , _inFile     :: String
    , _outFile    :: String
    , _operations :: [Operation]
    } deriving (Show, Generic)

makeLenses ''Job

addUnderscore :: String -> String
addUnderscore "" = ""
addUnderscore s  = '_' : s

instance A.FromJSON Job where
    parseJSON = A.genericParseJSON customOptions
instance A.ToJSON Job where
    toJSON = A.genericToJSON customOptions

data Config = Config
    { _jobs            :: [Job]
    , _numberOfThreads :: Int
    , _logFile         :: String
    } deriving (Show, Generic)

makeLenses ''Config

instance A.FromJSON Config where
    parseJSON = A.genericParseJSON customOptions
instance A.ToJSON Config where
    toJSON = A.genericToJSON customOptions

readConfigFromFile :: FilePath -> IO (Maybe Config)
readConfigFromFile filePath = do
    result <- A.eitherDecodeFileStrict filePath
    case result of
        Left err -> do
            putStrLn $ "Error parsing JSON: " ++ err
            pure Nothing
        Right job -> pure (Just job)

applyOperation :: Operation -> Double -> Double -> NumberOrErr
applyOperation op x y = case op of
    Add      -> pure $ x + y
    Subtract -> pure $ x - y
    Multiply -> pure $ x * y
    Divide   -> if y == 0
                then Left "Division by zero"
                else pure $ x / y

myOperations :: [Operation]
myOperations = cycle [Add, Subtract]
--myOperations = cycle [Add, Subtract, Multiply, Divide]

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
                yield $ Left $ "Failed to parse number: " <> T.unpack txt <> " Unparsed Leftover: " <> T.unpack l
                -- stop the processing
                pure ()
            Left err -> do
                yield $ Left $ "Failed to parse number: " <> T.unpack txt <> " Error: " <> err
                -- stop the processing
                pure ()
        Nothing -> pure ()

processNumbers :: [Operation] -> ConduitT NumberOrErr Void (ResourceT IO) Result
processNumbers ops = do
    mFirstNum <- await
    case mFirstNum of
        Nothing          -> pure $ Result Nothing ["No numbers in input file."]
        Just (Right num) -> processRest (Result (Just num) []) ops
        Just (Left err)  -> processRest (Result Nothing [err]) ops

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

--tasks :: [IO ()]
--tasks =
--    fmap ((\i -> processFile ("i" ++ i ++ ".txt") ("o" ++ i ++ ".txt")) . show) ([1..50] :: [Int])

chunk :: Int -> [a] -> [[a]]
chunk _ [] = []
chunk n xs = take n xs : chunk n (drop n xs)

main :: IO ()
main = do
    startTime <- getCurrentTime
    mConf <- readConfigFromFile "config.json"
    case mConf of
        Just conf -> do
            let numThreads = conf ^. numberOfThreads
                batches    = chunk numThreads $ map runJob $ conf ^. jobs

            mapM_ (\batch -> mapConcurrently_ (\p -> handle onErr p) batch) batches
            endTime <- getCurrentTime
            let elapsedTime = diffUTCTime endTime startTime
            putStrLn $ "Elapsed time: " ++ show elapsedTime
        Nothing -> putStrLn "Exiting: Error reading config..."

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

-- TODO ADD ID
onErr :: SomeException -> IO ()
onErr e = do
    putStrLn $ "action was killed by: " ++ displayException e

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


