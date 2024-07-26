{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Main where

import Data.Text qualified as T
import Data.String.Interpolate (i)

import Data.Function ((&))
import Text.Emoji.OpenMoji.Types (OpenMoji (_openMoji_tags, _openMoji_annotation, _openMoji_emoji))
import Text.Emoji.OpenMoji.Data (openmojis)
import qualified Text.Fuzzily as Fuzzy
import Data.List (sortBy)

import qualified GI.Gtk as Gtk (main, init)
import GI.Gtk
       (widgetShowAll, setContainerChild, widgetDestroy, onButtonClicked,
        setButtonLabel, buttonNew, setWindowTitle, setContainerBorderWidth,
        mainQuit, onWidgetDestroy, windowNew, searchBarNew, searchEntryNew, boxNew, Orientation (OrientationVertical, OrientationHorizontal), boxPackEnd, boxPackStart, Box, containerForeach, widgetSetTooltipText, onEntryBufferInsertedText, searchEntryHandleEvent, flowBoxInsert, flowBoxNew)
import GI.Gtk.Enums (WindowType(..))
import Control.Monad (forM, forM_)
import Data.List.Split (chunksOf)

-- | Fuzzy-find emojis by their annotation
fuzzyFindEmoji :: T.Text -> [OpenMoji]
fuzzyFindEmoji query = do
  let emojis = fuzzyFindEmoji' query
  let sorted = sortBy (\x y -> compare (Fuzzy.score x) (Fuzzy.score y)) emojis
  [Fuzzy.original x | x <- sorted]

fuzzyFindEmoji' :: T.Text -> [Fuzzy.Fuzzy OpenMoji T.Text]
fuzzyFindEmoji' query = Fuzzy.filter Fuzzy.IgnoreCase ("","") _openMoji_annotation query openmojis 

-- showResults :: T.Text -> Box -> _
showResults query flowbox n m = do
  -- 1. clear the box
  containerForeach flowbox widgetDestroy
  -- 2. get the results
  let results = fuzzyFindEmoji query & take (n * m)
  let rows = chunksOf m results
  -- 3. show the results
  forM results $ \emoji -> do
    button <- buttonNew
    setButtonLabel button (_openMoji_emoji emoji)
    flowBoxInsert flowbox button 0
  -- forM_ rows $ \row -> do
  --   rowBox <- boxNew OrientationHorizontal 10
  --   forM_ row $ \emoji -> do
  --     button <- buttonNew
  --     setButtonLabel button (_openMoji_emoji emoji)
  --     widgetSetTooltipText button (Just $ _openMoji_annotation emoji)
  --     boxPackStart rowBox button False False 0
  --   boxPackStart box rowBox False False 0
  

main :: IO ()
main = do
  Gtk.init Nothing
  -- Create a new window
  window <- windowNew WindowTypeToplevel
  onWidgetDestroy window mainQuit
  setContainerBorderWidth window 10
  setWindowTitle window "Emoji-Keyboard"

  -- create a new column
  root <- boxNew OrientationVertical 10

  -- Add a searchbar (for the search query)
  search <- searchEntryNew
  boxPackStart root search False False 0

  results <- flowBoxNew 
  boxPackStart root results False False 0

  showResults "hand" results 5 5

  setContainerChild window root

  widgetShowAll window
  Gtk.main
