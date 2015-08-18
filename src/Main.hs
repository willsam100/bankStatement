module Main where

import Bassbull

main :: IO ()
main = do
  spending <- getAtBatsSum "/Users/willsam100/Downloads/A0304420204078025-01Jun15.csv"
  putStrLn $ "Spending categories are: " ++ (show spending)

  --mapM_ (putStrLn . show) spending
