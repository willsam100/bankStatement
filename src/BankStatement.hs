{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}

module BankStatement where

import qualified Data.Text as T
import Data.Text.Encoding as E
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString as B
import qualified Data.Foldable as F
import Data.Csv.Streaming
import qualified Data.Map as Map

import System.Locale
import Data.Time
import Data.Time.Format

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

type Values = T.Text
type Amount = Double
type Classified = String
type IsClassified = Transaction -> Bool

categories :: [(Classified, IsClassified)]
categories = [("Income", isIncome)
              , ("Food", descriptionContainsWords ["Pak N Save", "Countdown"])
              , ("Rent", descriptionContainsWords ["7110Lachie"])
              , ("Isagenix", descriptionContainsWords ["Isagenix"])
              , ("Takeways", descriptionContainsWords ["Cafe", "Sierra", "sals","Genta","The Globe Bar","Burger King","Subway","McDonalds","Wendy's","Carls Jr","Burgerfuel","Burger Fuel","Fish & Chips","kfc","Kebabs","Pita Pit", "Starbucks", "Pizza", "La Porchet"])
              , ("Expenses", descriptionContainsWords ["Sklenars","Line of credit","AMI Insurance","Spotify Premium x 12","Professional Earcare","Just Cuts","Repco","Prepaid","Pb Technologies","Kiwivelo"])
              , ("Transport", descriptionContainsWords ["gull", "Tyres"])
              , ("Entertainment", descriptionContainsWords ["Event", "netflix"])
              ]

descriptionContainsWords :: [T.Text] -> IsClassified
descriptionContainsWords xs = (foldr (||) False) . (flip descriptionContains) (map T.toLower xs)

isIncome :: IsClassified
isIncome t 
    | amount t > 0 && (T.toLower . toText) t == "eroad limited" = True
    | otherwise                                        = False  

addToCategory :: String -> (Double, [T.Text]) -> Map.Map Classified (Amount, [T.Text]) -> Map.Map Classified (Amount, [T.Text])
addToCategory s d = Map.insertWith appendAndAdd s d
  where appendAndAdd a b = ((fst a) + (fst b), (snd a) ++ (snd b))

toCategory :: [(Classified, IsClassified)] -> Transaction -> Map.Map String (Amount, [T.Text]) -> Map.Map String (Amount, [T.Text])
toCategory [] t                         = addToCategory "Other" (amount t, [toText t])
toCategory (x:xs) t | (snd x) t == True = (addToCategory . fst) x (amount t, [toText t])
                    | otherwise         = toCategory xs t

transactions :: BL.ByteString -> Records Transaction
transactions = decode HasHeader

toText :: Transaction -> T.Text
toText = E.decodeUtf8 . BL.toStrict . description

descriptionContains :: Transaction -> [T.Text] -> [Bool]
descriptionContains = map . (flip T.isInfixOf) . T.toLower . toText

getAtBatsSum :: BL.ByteString -> (Map.Map String (Amount, [T.Text]))
getAtBatsSum csvData = F.foldr' (toCategory categories) Map.empty (transactions csvData)






