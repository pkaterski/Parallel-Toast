module Main21 (main) where

import Data.Conduit
import qualified Data.Conduit.Combinators as C
import qualified Data.Conduit.Text as CT
import qualified Data.Text as T
import qualified Data.Text.Read as TR
import System.Environment (getArgs)
import Control.Monad.IO.Class()
import Control.Monad.Trans.Resource (ResourceT)

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

processFiles :: FilePath -> FilePath -> IO ()
processFiles inputFile outputFile = do
    result <- runConduitRes $
        C.sourceFile inputFile
        .| CT.decodeUtf8
        .| C.concatMap T.words
        .| C.mapM parseNumber
        .| processNumbers
    case result of
        Left err -> putStrLn $ "[ERROR] " ++ err
        Right n  -> do
            writeFile outputFile (show n)
            putStrLn "Done."


