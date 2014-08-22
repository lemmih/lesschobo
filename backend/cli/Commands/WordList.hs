{-# LANGUAGE RecordWildCards #-}
module Commands.WordList
  ( cmdWordList ) where

import           Control.Monad
import           Data.Chinese.CCDict as CCDict
import           Data.List
import qualified Data.Text           as T
import qualified Data.Text.IO        as T
import           Text.Printf

cmdWordList :: FilePath -> IO ()
cmdWordList path = do
  text <- T.readFile path
  let entries = nub [ entry | KnownWord entry <- tokenizer ccDict text ]
  forM_ entries $ \Entry{..} ->
    forM_ (zip entryPinyin entryDefinition) $ \(pinyin, english) -> do
      printf "%s\t%-10s %s\n"
        (T.unpack entryChinese)
        (T.unpack pinyin)
        (merge english)

merge :: [T.Text] -> String
merge = concat . intersperse "/" . map T.unpack


