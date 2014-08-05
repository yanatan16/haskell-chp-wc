{-# LANGUAGE NoImplicitPrelude #-}

module Main where

import Prelude (String, ($), error)
import Control.Monad (Monad(..))
import Control.Applicative ((<$>))
import Data.Maybe (Maybe(..))
import Data.Monoid (Monoid(..), (<>))
import Data.List (map, elem)

import Data.WordCount.Types (Counter(..))
import Data.WordCount.CHP.Core (lineCounter, wordCounter, charCounter, byteCounter, CHPCounter)
import Data.WordCount.CHP.Files ()

import System.IO
import System.Environment (getArgs)

main = do
  (counter, files) <- parseArgs
  case counter of
    Nothing -> putStrLn "usage: wc-pipes [-lwcm] [file1 file2 ...]"
    Just cntr -> case files of
      [] -> countStdin cntr
      fs -> countFiles fs cntr

parseArgs :: IO (Maybe CHPCounter, [FilePath])
parseArgs = do
  args <- getArgs
  case args of
    ('-':opts):files -> return (optsToCounter opts, files)
    files -> return (Just defaultCounter, files)

defaultCounter = lineCounter <> wordCounter <> charCounter

optsToCounter :: String -> Maybe CHPCounter
optsToCounter s = mconcat $ map charMap s
  where
    charMap 'l' = Just lineCounter
    charMap 'w' = Just wordCounter
    charMap 'c' = Just charCounter
    charMap 'm' = Just $ if elem 'c' s then mempty else byteCounter
    charMap _ = Nothing
