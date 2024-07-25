{-# LANGUAGE DeriveGeneric #-}

module EmojiDatabase where

import GHC.Generics
import Data.Text (Text)
import qualified Data.Text as T
import Data.Aeson (FromJSON, decode)
import System.Environment (getExecutablePath)
import System.FilePath ((</>), takeDirectory)
import qualified Data.ByteString.Lazy as B

data Emoji = Emoji {
  emoji :: Text,
  description :: Text,
  category :: Text,
  aliases :: [Text],
  tags :: [Text],
  unicode_version :: Text,
  ios_version :: Text
} deriving (Generic, Show, Eq)

instance FromJSON Emoji

readEmojiDatabase :: IO (Maybe [Emoji])
readEmojiDatabase = readEmojiDatabase' =<< defaultEmojiDatabaseLocation

defaultEmojiDatabaseLocation :: IO FilePath
defaultEmojiDatabaseLocation = do
  exeDir <- takeDirectory <$> getExecutablePath
  return $ exeDir </> "../assets/emoji.json"

readEmojiDatabase' :: FilePath -> IO (Maybe [Emoji])
readEmojiDatabase' path = do
  -- 1. read the file contents as a string
  str <- B.readFile path
  -- 2. parse the string as a list of Emoji
  let emojis = decode str :: Maybe [Emoji]
  return emojis

-- | Query the emoji database for emojis that have some text in their description
queryEmojiDatabase :: Text -> [Emoji] -> [Emoji]
queryEmojiDatabase q = filter (\e -> q `elem` T.words (description e))
