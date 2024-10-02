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

tasks :: [IO ()]
tasks =
    [ processFile "i1.txt" "o1.txt"
    , processFile "i1.txt" "o1.txt"
    , processFile "i2.txt" "o2.txt"
    , processFile "i3.txt" "o3.txt"
    , processFile "i4.txt" "o4.txt"
    , processFile "i5.txt" "o5.txt"
    , processFile "i6.txt" "o6.txt"
    , processFile "i7.txt" "o7.txt"
    , processFile "i8.txt" "o8.txt"
    , processFile "i9.txt" "o9.txt"
    , processFile "i10.txt" "o10.txt"
    , processFile "i11.txt" "o11.txt"
    , processFile "i12.txt" "o12.txt"
    , processFile "i13.txt" "o13.txt"
    , processFile "i14.txt" "o14.txt"
    , processFile "i15.txt" "o15.txt"
    , processFile "i16.txt" "o16.txt"
    , processFile "i17.txt" "o17.txt"
    , processFile "i18.txt" "o18.txt"
    , processFile "i19.txt" "o19.txt"
    , processFile "i20.txt" "o20.txt"
    , processFile "i21.txt" "o21.txt"
    , processFile "i22.txt" "o22.txt"
    , processFile "i23.txt" "o23.txt"
    , processFile "i24.txt" "o24.txt"
    , processFile "i25.txt" "o25.txt"
    , processFile "i26.txt" "o26.txt"
    , processFile "i27.txt" "o27.txt"
    , processFile "i28.txt" "o28.txt"
    , processFile "i29.txt" "o29.txt"
    , processFile "i30.txt" "o30.txt"
    , processFile "i31.txt" "o31.txt"
    , processFile "i32.txt" "o32.txt"
    , processFile "i33.txt" "o33.txt"
    , processFile "i34.txt" "o34.txt"
    , processFile "i35.txt" "o35.txt"
    , processFile "i36.txt" "o36.txt"
    , processFile "i37.txt" "o37.txt"
    , processFile "i38.txt" "o38.txt"
    , processFile "i39.txt" "o39.txt"
    , processFile "i40.txt" "o40.txt"
    , processFile "i41.txt" "o41.txt"
    , processFile "i42.txt" "o42.txt"
    , processFile "i43.txt" "o43.txt"
    , processFile "i44.txt" "o44.txt"
    , processFile "i45.txt" "o45.txt"
    , processFile "i46.txt" "o46.txt"
    , processFile "i47.txt" "o47.txt"
    , processFile "i48.txt" "o48.txt"
    , processFile "i49.txt" "o49.txt"
    , processFile "i50.txt" "o50.txt"
    ]

main :: IO ()
main = do
    startTime <- getCurrentTime

    mapConcurrently_ id tasks
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

-- TODO refactor!
-- first arg is a leftover from the previous stream
parseWords :: Monad m => Maybe T.Text -> ConduitT T.Text T.Text m ()
parseWords (Just l) = do
    str <- await
    case str of
        Just s -> do
            let (ws, l') = popLast $ T.words $ l <> s
            C.yieldMany ws
            if T.last s == '\n'
                then do
                    case l' of
                        Just l'' -> yield l''
                        Nothing -> pure ()
                    parseWords Nothing
                else parseWords l'
        Nothing -> do
            yield l
            pure ()
parseWords Nothing = do
    str <- await
    case str of
        Just s -> do
            let (ws, l') = popLast $ T.words s
            C.yieldMany ws
            if T.last s == '\n'
                then do
                    case l' of
                        Just l'' -> yield l''
                        Nothing -> pure ()
                    parseWords Nothing
                else parseWords l'
        Nothing -> pure ()

processFile :: FilePath -> FilePath -> IO ()
processFile inputFile outputFile = do
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
            putStrLn $ "Done: " <> outputFile


