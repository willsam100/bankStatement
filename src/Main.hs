{-# LANGUAGE OverloadedStrings #-}

module Main where

import BankStatement
import qualified Data.Map as Map
import qualified Data.Text as T
import qualified Data.ByteString.Lazy as BL
import System.FilePath.Posix

--import Data.List.Utils

main :: IO ()
main = do
  processFiles ["/Users/willsam100/Downloads/June 2015.csv"
              , "/Users/willsam100/Downloads/July 2015.csv"
              , "/Users/willsam100/Downloads/August 2015.csv"]

processFiles :: [FilePath] -> IO ()
processFiles = mapM_ processFile 

processFile :: FilePath -> IO ()
processFile filename = do 
  putStrLn $ takeBaseName filename
  bs <- BL.readFile filename
  let results = Map.toList $ getAtBatsSum bs
      balance = "balance " ++  (show $ Prelude.foldr (\(_, (x, _)) a -> a + x) 0 results)
  putStrLn balance
  mapM_ (putStrLn . showResults) results
  where 
    showResults (x, y) = "\t" ++ x ++ ": " ++ (showNumber (show (fst y))) ++ (Prelude.foldr (++) "" (listTransactions (snd y)))
    showNumber = takeWhile (\x -> x /= '.')
    listTransactions = map (\t -> "\n\t\t" ++  (T.unpack t))