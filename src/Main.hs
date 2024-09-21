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
import GI.Gdk (keyvalName, keyvalToUnicode)
import GI.Gdk.Objects.Window (windowSetGeometryHints)
import GI.Gtk (Box, Orientation (OrientationHorizontal, OrientationVertical), afterSearchEntrySearchChanged, afterSearchEntryStopSearch, boxNew, boxPackEnd, boxPackStart, buttonNew, containerAdd, containerForeach, entryGetText, flowBoxInsert, flowBoxNew, mainQuit, onButtonClicked, onEntryBufferInsertedText, onSearchEntrySearchChanged, onWidgetDestroy, onWidgetKeyPressEvent, searchBarNew, searchEntryHandleEvent, searchEntryNew, setButtonLabel, setContainerBorderWidth, setContainerChild, setWindowTitle, widgetDestroy, widgetSetTooltipText, widgetShowAll, windowNew, windowResize, windowSetDecorated, windowSetResizable, windowSetTypeHint)
import GI.Gtk qualified as Gtk (get, init, main)
import GI.Gtk.Enums (WindowType (..))
import GI.Gtk.Objects (FlowBox)
import GI.Gtk.Objects.Window (windowSetDefaultSize)
import System.Process (readProcess, readProcessWithExitCode)
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

showResults :: T.Text -> Box -> Int -> Int -> (Fuzzy.Fuzzy OpenMoji T.Text -> IO ()) -> IO ()
showResults query box m n onClick = do
  -- 1. clear the gtk box (here we will place the results)
  containerForeach box widgetDestroy
  -- 2. get the results
  let results = fuzzyFindEmojiSorted query & take (m * n)
  let rows = chunksOf m results
  -- 3. show the results into the gtk box
  forM_ rows $ \row -> do
    rowBox <- boxNew OrientationHorizontal 10
    boxPackStart box rowBox False False 0
    forM_ row $ \emoji -> do
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
      containerAdd rowBox button
  widgetShowAll box

typeText :: String -> T.Text -> IO ()
typeText windowIdToTypeInfo text = do
  -- Get the ID of the emoji-keyboard window (after it has been created and is
  -- therefor focused). We will need this value, when we try to refocus on the
  -- emoji-keyboard after we have typed some text into `focusedWindowID`.
  emojiKeyboardWindowID <- head . lines <$> readProcess "kdotool" ["getactivewindow"] ""
  let
    focus = readProcess "kdotool" ["windowactivate", windowIdToTypeInfo] ""
    refocus = readProcess "kdotool" ["windowactivate", emojiKeyboardWindowID] ""
    copy = readProcess "wl-copy" [] $ T.unpack text
    -- simulate `ctrl+shift+v
    paste = readProcess "ydotool" ["key", "29:1", "42:1", "47:1", "47:0", "42:0", "29:0"] ""
   in
    copy >> focus >> paste >> refocus & void

main :: IO ()
main = do
  -- Get the ID of the currently focused window (before we open up the
  -- emoji-keyboard window).
  -- We do this, so we can focus on it during `typeText`!
  focusedWindowID <- head . lines <$> readProcess "kdotool" ["getactivewindow"] ""
  Gtk.init Nothing
  -- Create a new window
  window <- windowNew WindowTypeToplevel
  windowSetDefaultSize window 500 250
  -- mark the window such that it will not be tiled by the window manager
  onWidgetDestroy window mainQuit
  setContainerBorderWidth window 10
  setWindowTitle window "Emoji-Keyboard"

  -- Add key listener for Escape key to quit application
  onWidgetKeyPressEvent window $ \event -> do
    keyval <- keyvalName =<< keyvalToUnicode =<< Gtk.get event #keyval
    case keyval of
      -- escape key pressed
      Just "0x1b" -> mainQuit >> return True
      _ -> return False

  -- create a new column
  root <- boxNew OrientationVertical 10

  -- Add a searchbar (for the search query)
  search <- searchEntryNew
  boxPackStart root search False False 0

  results <- boxNew OrientationVertical 10
  boxPackStart root results False False 0

  showResults "smile" results 10 5 $ \e ->
    typeText focusedWindowID (_openMoji_emoji (Fuzzy.original e))
  afterSearchEntrySearchChanged search $ do
    -- get the search entries text
    query <- entryGetText search
    showResults query results 10 5 $ \e ->
      typeText focusedWindowID (_openMoji_emoji (Fuzzy.original e))

  setContainerChild window root

  widgetShowAll window
  Gtk.main
