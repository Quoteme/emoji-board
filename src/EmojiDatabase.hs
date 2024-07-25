{-# LANGUAGE DeriveGeneric #-}

module EmojiDatabase where

import GHC.Generics
import Data.Aeson (FromJSON, decode)
import System.Environment (getExecutablePath)
import System.FilePath ((</>), takeDirectory)
import qualified Data.ByteString.Lazy as B

data Emoji = Emoji {
  emoji :: String,
  description :: String,
  category :: String,
  aliases :: [String],
  tags :: [String],
  unicode_version :: String,
  ios_version :: String
} deriving (Generic, Show)

instance FromJSON Emoji

defaultEmojiDatabaseLocation :: IO FilePath
defaultEmojiDatabaseLocation = do
  exeDir <- takeDirectory <$> getExecutablePath
  return $ exeDir </> "./assets/emoji.json"

readEmojiDatabase' :: FilePath -> IO (Maybe [Emoji])
readEmojiDatabase' path = do
  -- 1. read the file contents as a string
  str <- B.readFile path
  -- 2. parse the string as a list of Emoji
  let emojis = decode str :: Maybe [Emoji]
  return emojis
