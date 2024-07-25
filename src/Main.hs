{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Main where

import Data.Text qualified as T
import Data.String.Interpolate (i)
import Monomer.Lens qualified as L

import Control.Lens
import Data.Text (Text)
import Monomer
import TextShow

import qualified Monomer.Lens as L
import System.Environment (getExecutablePath)
import System.FilePath (takeDirectory)
import Control.Monad (when)
import System.Exit (exitFailure)
import Data.Maybe (isNothing, fromJust)
import Text.Emoji.OpenMoji.Types (OpenMoji (_openMoji_tags, _openMoji_annotation, _openMoji_emoji))
import Text.Emoji.OpenMoji.Data (openmojis)
import qualified Text.Fuzzily as Fuzzy
import Data.List.Extra (sortBy)

data AppModel = AppModel {
  _query :: Text,
  _emojis :: [OpenMoji]
} deriving (Eq, Show)

data AppEvent
  = AppInit
  | AppSearch Text
  deriving (Eq, Show)

makeLenses 'AppModel

buildUI
  :: WidgetEnv AppModel AppEvent
  -> AppModel
  -> WidgetNode AppModel AppEvent
buildUI wenv model = widgetTree where
  searchForm = vstack [
      hstack [
        textField query `nodeKey` "query",
        button "SEARCH!" $ AppSearch (model ^. query)
      ] `styleBasic` [padding 25]
    ]
  widgetTree = vstack [
      searchForm,
      spacer,
      vstack [ 
        hstack [
          label (_openMoji_emoji x) `styleBasic` [textFont "Emoji", padding 10],
          label (_openMoji_emoji x) `styleBasic` [textFont "EmojiNoColor", padding 10],
          label (_openMoji_annotation x) `styleBasic` [textFont "Regular", padding 10]
          ]
        | x <- model ^. emojis 
        ],
      label $ "🤓 Found emojis: " <> showt (length (model ^. emojis))
    ] `styleBasic` [padding 10]

handleEvent
  :: WidgetEnv AppModel AppEvent
  -> WidgetNode AppModel AppEvent
  -> AppModel
  -> AppEvent
  -> [AppEventResponse AppModel AppEvent]
handleEvent wenv node model evt = case evt of
  AppInit -> []
  AppSearch s -> [
    Model (model & query .~ s & emojis .~ fuzzyFindEmoji s)
    ]

main :: IO ()
main = do
  exeDir <- takeDirectory <$> getExecutablePath
  let
    config = [
      appWindowTitle "Emoji-Keyboard",
      appWindowIcon "./assets/images/icon.png",
      appTheme darkTheme,
      appFontDef "Emoji" [i|#{exeDir}/../assets/fonts/NotoColorEmoji-Regular.ttf |],
      appFontDef "EmojiNoColor" [i|#{exeDir}/../assets/fonts/NotoEmoji-Regular.ttf |],
      appFontDef "Regular" [i|#{exeDir}/../assets/fonts/Roboto-Regular.ttf|],
      appFontDef "Medium" [i|#{exeDir}/../assets/fonts/Roboto-Medium.ttf|],
      appFontDef "Bold" [i|#{exeDir}/../assets/fonts/Roboto-Bold.ttf|],
      appFontDef "Remix" [i|#{exeDir}/../assets/fonts/remixicon.ttf|],
      appInitEvent AppInit
      ]
    model = AppModel "" []
  startApp model handleEvent buildUI config

-- | Fuzzy-find emojis by their annotation
fuzzyFindEmoji :: Text -> [OpenMoji]
fuzzyFindEmoji query = do
  let emojis = fuzzyFindEmoji' query
  let sorted = sortBy (\x y -> compare (Fuzzy.score x) (Fuzzy.score y)) emojis
  [Fuzzy.original x | x <- sorted]

fuzzyFindEmoji' :: Text -> [Fuzzy.Fuzzy OpenMoji Text]
fuzzyFindEmoji' query = Fuzzy.filter Fuzzy.IgnoreCase ("","") _openMoji_annotation query openmojis 
