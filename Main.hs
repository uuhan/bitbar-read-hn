{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE QuasiQuotes         #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import           Control.Exception
import           Control.Lens
import           Control.Monad
import           Data.Aeson
import qualified Data.ByteString.Lazy.Char8    as BS
import           Data.Maybe                    (catMaybes, fromMaybe)
import           Foreign.C.Types               (CTime (..))
import           GHC.Generics                  (Generic)
import           Network.Wreq                  as W
import           System.Directory
import           System.Environment            (getEnv)
import           System.FilePath
import           System.IO
import           System.Posix.Files
import           System.Posix.Time
import           Text.InterpolatedString.Perl6 (qq)

data Story = Story
           { id          :: Int
           , url         :: Maybe String
           , title       :: String
           , score       :: Int
           , descendants :: Maybe Int
           } deriving (Show, Generic)

instance FromJSON Story
instance ToJSON Story

numberOfNews :: Int
numberOfNews = 15

type Total = Int

main :: IO ()
main = do
    store_file <- storageFile
    refreshStore
    stories <- loadData
    let total = length stories
    cn <- currentNewsNo total
    let Story{..} = stories !! cn
    putStrLn [qq|🗞  {title}|]
    putStrLn "---"
    flip mapM_ stories $ \Story{..} -> do
      let comments_url = [qq|'https://news.ycombinator.com/item?id={id}'|]
      putStrLn [qq|{title} | href='{fromMaybe comments_url url}' color=black|]
      putStrLn [qq|Score: {score} Comments: {fromMaybe 0 descendants} | href='{comments_url}' color=#FF6600|]

fileTimeOut :: FilePath -> IO Bool
fileTimeOut f = do
    mtime   <- modificationTime <$> getFileStatus f
    curTime <- epochTime
    return $ (curTime - mtime) > storageTimeOut
  where
    storageTimeOut = 180

currentNewsNo :: Total -> IO Int
currentNewsNo total = do
    CTime t <- epochTime
    return $ (fromEnum t) `div` 60 `mod` total

refreshStore :: IO ()
refreshStore = do
    store_file <- storageFile
    timeout <- fileTimeOut store_file
    stories <- loadData

    when (length stories == 0 || timeout) persist

loadData :: IO [Story]
loadData = do
    store_file <- storageFile
    catMaybes . map decode . BS.lines <$> BS.readFile store_file

storageFile :: IO FilePath
storageFile = do
    file <- pure . (</> "hacker_news.txt") =<< getEnv "HOME"
    exists <- doesFileExist file
    unless exists $ writeFile file ""
    return file

persist :: IO ()
persist = do
    file <- storageFile
    ids <- fetchIds
    bracket
      (openFile file WriteMode >>= \handle -> do
        hSetBuffering handle LineBuffering
        return handle)
      (hClose)
      (\handle -> flip mapM_ ids $ \id -> do
        fetchStoryById id >>= pure . encode >>= BS.hPutStrLn handle
      )

fetchIds :: IO [Int]
fetchIds = do
    let opts = defaults & proxy ?~ httpProxy "localhost" 1111
    r <- asJSON =<< getWith opts url
    return $ take numberOfNews $ r ^. responseBody
  where
    url = "https://hacker-news.firebaseio.com/v0/topstories.json"

fetchStoryById :: Int -> IO Story
fetchStoryById idx = do
    let opts = defaults & proxy ?~ httpProxy "localhost" 1111
    r <- asJSON =<< getWith opts url
    return $ r ^. responseBody
  where
    url = [qq|https://hacker-news.firebaseio.com/v0/item/{idx}.json|]
