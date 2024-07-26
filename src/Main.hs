{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE QuasiQuotes #-}

module Main where

import Data.Text qualified as T
import Data.String.Interpolate (i)

import Data.Function ((&))
import Text.Emoji.OpenMoji.Types (OpenMoji (_openMoji_tags, _openMoji_annotation, _openMoji_emoji, _openMoji_order))
import Text.Emoji.OpenMoji.Data (openmojis)
import qualified Text.Fuzzily as Fuzzy
import Data.List (sortBy, intercalate)

import qualified GI.Gtk as Gtk (main, init)
import GI.Gtk
       (widgetShowAll, setContainerChild, widgetDestroy, onButtonClicked,
        setButtonLabel, buttonNew, setWindowTitle, setContainerBorderWidth,
        mainQuit, onWidgetDestroy, windowNew, searchBarNew, searchEntryNew, boxNew, Orientation (OrientationVertical, OrientationHorizontal), boxPackEnd, boxPackStart, Box, containerForeach, widgetSetTooltipText, onEntryBufferInsertedText, searchEntryHandleEvent, flowBoxInsert, flowBoxNew, onSearchEntrySearchChanged, entryGetText, afterSearchEntryStopSearch, afterSearchEntrySearchChanged, containerAdd)
import GI.Gtk.Enums (WindowType(..))
import Control.Monad (forM, forM_, void)
import Data.List.Split (chunksOf)
import Data.Maybe (fromMaybe, listToMaybe, catMaybes, mapMaybe)

-- | Fuzzy-find emojis by their annotation
fuzzyFindEmojiSorted :: T.Text -> [Fuzzy.Fuzzy OpenMoji T.Text]
fuzzyFindEmojiSorted query = do
  let emojis = fuzzyFindEmoji query
  let weightFunc x = Fuzzy.score x
  sortBy (\x y -> compare (weightFunc y) (weightFunc x)) emojis

fuzzyFindEmoji  :: T.Text -> [Fuzzy.Fuzzy OpenMoji T.Text]
fuzzyFindEmoji query = mapMaybe (doesEmojiMatch query) openmojis
  where
    makeEmojiSearchable :: OpenMoji -> [(T.Text, OpenMoji)]
    makeEmojiSearchable emoji = [(tag, emoji) | tag <- _openMoji_tags emoji] ++ [(_openMoji_annotation emoji, emoji)]
    -- doesEmojiMatch :: OpenMoji -> Maybe (Fuzzy.Fuzzy OpenMoji T.Text)
    doesEmojiMatch :: T.Text -> OpenMoji -> Maybe (Fuzzy.Fuzzy OpenMoji T.Text)
    doesEmojiMatch query emoji = listToMaybe . catMaybes $
      [Fuzzy.match Fuzzy.IgnoreCase ("<", ">") (const text) query emoji | (text, emoji) <- makeEmojiSearchable emoji]

generateTuples :: [[a]] -> [(a, [a])]
generateTuples = concatMap (\xs -> [(x, xs) | x <- xs])

-- showResults :: T.Text -> Box -> _
showResults query flowbox n = do
  -- 1. clear the box
  containerForeach flowbox widgetDestroy
  -- 2. get the results
  let results = fuzzyFindEmojiSorted query & take n
  -- 3. show the results
  forM_ results $ \emoji -> do
    button <- buttonNew
    setButtonLabel button (_openMoji_emoji $ Fuzzy.original emoji)
    widgetSetTooltipText button (Just $ T.unlines [
      _openMoji_annotation $ Fuzzy.original emoji,
      "Tags: " <> T.intercalate ", " (_openMoji_tags $ Fuzzy.original emoji),
      "Score: " <> T.pack (show $ Fuzzy.score emoji),
      "Match: " <> T.pack (show $ Fuzzy.rendered emoji)
      ])
    containerAdd flowbox button
  putStrLn [i|Showing #{length results} results for '#{query}'|]
  widgetShowAll flowbox

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

  showResults "smile" results 30
  afterSearchEntrySearchChanged search $ do
    -- get the search entries text
    query <- entryGetText search
    showResults query results 30

  setContainerChild window root

  widgetShowAll window
  Gtk.main
