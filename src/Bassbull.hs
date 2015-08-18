{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}

module Bassbull where

import qualified Data.Text as T
import Data.Text.Encoding as E
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString as B
import qualified Data.Foldable as F
import Data.Csv.Streaming
import qualified Data.Map as Map

import Data.Csv hiding (decode)
import GHC.Generics

data Transaction = Transaction {  date :: BL.ByteString 
                                , amount :: Double 
                                , description :: BL.ByteString
                                , otherParty :: BL.ByteString
                                , reference :: BL.ByteString
                                , particulars :: BL.ByteString
                                , analysisCode :: BL.ByteString
                                } deriving (Show, Generic)

instance FromRecord Transaction 

type Amount = Double
type Classified = String
type IsClassified = Transaction -> Bool

empty dollar desc = Transaction "" dollar desc "" "" "" ""

categories :: [(Classified, IsClassified)]
categories = [("Food", descriptionContainsWords ["caffe"]), ("Income", isIncome)]

descriptionContainsWords :: [T.Text] -> IsClassified
descriptionContainsWords xs = (foldr (||) False) . (flip descriptionContains) xs

isIncome :: IsClassified
isIncome t 
    | amount t > 0 = True
    | otherwise    = False


addToCategory :: String -> Double -> Map.Map Classified Amount -> Map.Map Classified Amount
addToCategory s = Map.insertWith (+) s

toCategory :: [(Classified, IsClassified)] -> Transaction -> Map.Map String Double -> Map.Map String Double
toCategory [] t                         = addToCategory "Other" (amount t)
toCategory (x:xs) t | (snd x) t == True = (addToCategory . fst) x (amount t)
                    | otherwise         = toCategory xs t

transactions :: BL.ByteString -> Records Transaction
transactions = decode HasHeader

descriptionContains :: Transaction -> [T.Text] -> [Bool]
descriptionContains = map . T.isInfixOf . T.toLower .  E.decodeUtf8 . BL.toStrict . description

-- FilePath is just an alias for String
getAtBatsSum :: FilePath -> IO (Map.Map String Double)
--getAtBatsSum = undefined
getAtBatsSum battingCsv = do
  csvData <- BL.readFile battingCsv
  return $ F.foldr' (toCategory categories) Map.empty (transactions csvData)





