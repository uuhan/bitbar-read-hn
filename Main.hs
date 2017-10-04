module Main where

import           Network.Wreq       as W
import           System.Environment (getEnv)
import           System.FilePath

main :: IO ()
main = do
    storageFile >>= putStrLn
  where
    storageFile = pure . (</> "hacker_news_data.txt") =<< getEnv "HOME"
