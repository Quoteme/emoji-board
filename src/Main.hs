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

import qualified EmojiDatabase as ED

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

data AppModel = AppModel {
  _query :: Text,
  _emojis :: [ED.Emoji],
  _foundEmojis :: [ED.Emoji]
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
      hstack [ label (ED.emoji x) | x <- model ^. foundEmojis ],
      label $ "Total emojis: 🤓" <> showt (length (model ^. emojis)),
      label $ "Found " <> showt (length (model ^. foundEmojis)) <> " emojis"
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
    Model (model & query .~ s & foundEmojis .~ ED.queryEmojiDatabase s (model ^. emojis))
    ]

main :: IO ()
main = do
  exeDir <- takeDirectory <$> getExecutablePath
  emojis <- fromJust <$> ED.readEmojiDatabase
  let
    config = [
      appWindowTitle "Emoji-Keyboard",
      appWindowIcon "./assets/images/icon.png",
      appTheme darkTheme,
      appFontDef "Regular" [i|#{exeDir}/../assets/fonts/Roboto-Regular.ttf|],
      appFontDef "Medium" [i|#{exeDir}/../assets/fonts/Roboto-Medium.ttf|],
      appFontDef "Bold" [i|#{exeDir}/../assets/fonts/Roboto-Bold.ttf|],
      appFontDef "Remix" [i|#{exeDir}/../assets/fonts/remixicon.ttf|],
      appInitEvent AppInit
      ]
    model = AppModel "" emojis []
  startApp model handleEvent buildUI config
