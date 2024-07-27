{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Data.String.Interpolate (i)
import Data.Text qualified as T

import Data.Function ((&))
import Data.List (intercalate, sortBy)
import Text.Emoji.OpenMoji.Data (openmojis)
import Text.Emoji.OpenMoji.Types (OpenMoji (_openMoji_annotation, _openMoji_emoji, _openMoji_order, _openMoji_tags))
import Text.Fuzzily qualified as Fuzzy

import Control.Concurrent (forkIO, threadDelay)
import Control.DeepSeq (deepseq)
import Control.Monad (forM, forM_, void)
import Data.List.Split (chunksOf)
import Data.Maybe (catMaybes, fromMaybe, listToMaybe, mapMaybe)
import GI.Gtk (
  Box,
  Orientation (OrientationHorizontal, OrientationVertical),
  afterSearchEntrySearchChanged,
  afterSearchEntryStopSearch,
  boxNew,
  boxPackEnd,
  boxPackStart,
  buttonNew,
  containerAdd,
  containerForeach,
  entryGetText,
  flowBoxInsert,
  flowBoxNew,
  mainQuit,
  onButtonClicked,
  onEntryBufferInsertedText,
  onSearchEntrySearchChanged,
  onWidgetDestroy,
  searchBarNew,
  searchEntryHandleEvent,
  searchEntryNew,
  setButtonLabel,
  setContainerBorderWidth,
  setContainerChild,
  setWindowTitle,
  widgetDestroy,
  widgetSetTooltipText,
  widgetShowAll,
  windowNew,
 )
import GI.Gtk qualified as Gtk (init, main)
import GI.Gtk.Enums (WindowType (..))
import System.Process (readProcessWithExitCode)
import System.Process.Typed (readProcessStdout)

-- | Fuzzy-find emojis by their annotation
fuzzyFindEmojiSorted :: T.Text -> [Fuzzy.Fuzzy OpenMoji T.Text]
fuzzyFindEmojiSorted query = do
  let emojis = fuzzyFindEmoji query
  let weightFunc x = Fuzzy.score x
  sortBy (\x y -> compare (weightFunc y) (weightFunc x)) emojis

fuzzyFindEmoji :: T.Text -> [Fuzzy.Fuzzy OpenMoji T.Text]
fuzzyFindEmoji query = mapMaybe (doesEmojiMatch query) openmojis
 where
  makeEmojiSearchable :: OpenMoji -> [(T.Text, OpenMoji)]
  makeEmojiSearchable emoji = [(tag, emoji) | tag <- _openMoji_tags emoji] ++ [(_openMoji_annotation emoji, emoji)]
  -- doesEmojiMatch :: OpenMoji -> Maybe (Fuzzy.Fuzzy OpenMoji T.Text)
  doesEmojiMatch :: T.Text -> OpenMoji -> Maybe (Fuzzy.Fuzzy OpenMoji T.Text)
  doesEmojiMatch query emoji =
    listToMaybe . catMaybes $
      [Fuzzy.match Fuzzy.IgnoreCase ("<", ">") (const text) query emoji | (text, emoji) <- makeEmojiSearchable emoji]

generateTuples :: [[a]] -> [(a, [a])]
generateTuples = concatMap (\xs -> [(x, xs) | x <- xs])

-- showResults :: T.Text -> Box -> _
showResults query flowbox n onClick = do
  -- 1. clear the box
  containerForeach flowbox widgetDestroy
  -- 2. get the results
  let results = fuzzyFindEmojiSorted query & take n
  -- 3. show the results
  forM_ results $ \emoji -> do
    button <- buttonNew
    setButtonLabel button (_openMoji_emoji $ Fuzzy.original emoji)
    onButtonClicked button $ onClick emoji
    widgetSetTooltipText
      button
      ( Just $
          T.unlines
            [ _openMoji_annotation $ Fuzzy.original emoji
            , "Tags: " <> T.intercalate ", " (_openMoji_tags $ Fuzzy.original emoji)
            , "Score: " <> T.pack (show $ Fuzzy.score emoji)
            , "Match: " <> T.pack (show $ Fuzzy.rendered emoji)
            ]
      )
    containerAdd flowbox button
  putStrLn [i|Showing #{length results} results for '#{query}'|]
  widgetShowAll flowbox

main :: IO ()
main = do
  (_, active, _) <- readProcessWithExitCode "kdotool" ["getactivewindow"] ""
  putStrLn active
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

  let typeText :: T.Text -> IO ()
      typeText text = do
        let activate = do
              putStrLn $ "Activating " <> active
              readProcessWithExitCode "kdotool" ["windowactivate", active] ""
        let typing = do
              putStrLn $ "Typing " <> T.unpack text
              readProcessWithExitCode "ydotool" ["type", "--delay=50", T.unpack text] ""
        void $ activate `seq` typing
  -- widgetDestroy window

  showResults "smile" results 30 $ \e ->
    typeText (_openMoji_emoji (Fuzzy.original e))
  afterSearchEntrySearchChanged search $ do
    -- get the search entries text
    query <- entryGetText search
    showResults query results 30 $ \e ->
      typeText (_openMoji_emoji (Fuzzy.original e))

  setContainerChild window root

  widgetShowAll window
  Gtk.main
