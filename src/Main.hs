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

newtype AppModel = AppModel {
  _query :: Text
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
        label "Query:",
        spacer,
        textField query `nodeKey` "query",
        spacer
      ] `styleBasic` [padding 25]
    ]
  widgetTree = vstack [
      searchForm,
      spacer,
      hstack [
        label $ "Click count: " <> showt (model ^. query),
        button "Increase count" $ AppSearch "test"
      ]
    ] `styleBasic` [padding 10]

handleEvent
  :: WidgetEnv AppModel AppEvent
  -> WidgetNode AppModel AppEvent
  -> AppModel
  -> AppEvent
  -> [AppEventResponse AppModel AppEvent]
handleEvent wenv node model evt = case evt of
  AppInit -> []
  AppSearch s -> [Model (model & query .~ s)]

main :: IO ()
main = do
  exeDir <- takeDirectory <$> getExecutablePath
  putStrLn [i|Starting app from: #{exeDir}|]
  startApp model handleEvent buildUI (config exeDir)
  where
    config exeDir = [
      appWindowTitle "Emoji-Keyboard",
      appWindowIcon "./assets/images/icon.png",
      appTheme darkTheme,
      appFontDef "Regular" [i|#{exeDir}/../assets/fonts/Roboto-Regular.ttf|],
      appFontDef "Medium" [i|#{exeDir}/../assets/fonts/Roboto-Medium.ttf|],
      appFontDef "Bold" [i|#{exeDir}/../assets/fonts/Roboto-Bold.ttf|],
      appFontDef "Remix" [i|#{exeDir}/../assets/fonts/remixicon.ttf|],
      appInitEvent AppInit
      ]
    model = AppModel ""
