{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Data.WordCount.CHP.Core (
  lineCounter,
  wordCounter,
  charCounter,
  byteCounter,
  CHPCounter,
  runCounter
)
where

import Data.WordCount.Types (Counts(..), CountsUpdater, upLines, upWords, upChars, upBytes)

import Prelude (Char, String, IO, Show(..), error, (.), ($), (+), (++), length, max, map, zipWith3, words, lines, take)

import Data.Word (Word8)
import Data.ByteString.Lazy (ByteString, unpack) -- unpack :: ByteString -> [Word8]
import Data.ByteString.Lazy.Char8 (pack)         -- pack :: String -> ByteString

import Data.Maybe (Maybe(..))
import Data.Monoid (Monoid(..))
import Data.Traversable ()
import Control.Monad (mapM, mapM_, replicateM, forever, Monad(..))
import System.IO (putStrLn)

import Control.Concurrent.CHP
import qualified Control.Concurrent.CHP.Common as CHP
import Control.Concurrent.CHP.Connect

newtype CHPCounter = CHPCounter { runCHPCounter :: Chanin String -> Chanout Counts -> CHP () }

instance Monoid CHPCounter where
  mempty = CHPCounter $ \_ _ -> return ()
  mappend = combineCounters

runCounter :: CHPCounter -> String -> Chanout Counts -> CHP ()
runCounter cc s cout = ((producer s |<=> runCHPCounter cc) cout)
  `onPoisonTrap` return ()


producer :: a -> Chanout a -> CHP ()
producer a c = writeChannel c a >> poison c

-- |
-- | Counting Funcs
-- |

lineCounter :: CHPCounter
lineCounter = CHPCounter $ lines' <=> counter
  where counter = count upLines :: Chanin String -> Chanout Counts -> CHP ()

wordCounter :: CHPCounter
wordCounter = CHPCounter $ words' <=> counter
  where counter = count upWords :: Chanin String -> Chanout Counts -> CHP ()

charCounter :: CHPCounter
charCounter = CHPCounter $ chars' <=> counter
  where counter = count upChars :: Chanin Char -> Chanout Counts -> CHP ()

byteCounter :: CHPCounter
byteCounter = CHPCounter $ bytes' <=> counter
  where counter = count upBytes :: Chanin Word8 -> Chanout Counts -> CHP ()

-- |
-- | Counting Core
-- |

-- | run two counters in parallel and combine the results using
combineCounters :: CHPCounter -> CHPCounter -> CHPCounter
combineCounters c1 c2 = CHPCounter $ parTee $ map runCHPCounter [c1, c2]

-- | parallel tee of a channel to N monoid-value-producing consumers
parTee :: (Show b, Monoid b) => [Chanin a -> Chanout b -> CHP ()] -> Chanin a -> Chanout b -> CHP ()
parTee ps cin cout = do
  cmid1s <- newChannelListWithLabels (map (("parTee1-" ++) . show) [1..(length ps)])
  cmid2s <- newChannelListWithLabels (map (("parTee2-" ++) . show) [1..(length ps)])
  let split = CHP.parDelta cin $ writers cmid1s
  let procs = zipWith3 (\p r w -> p r w) ps (readers cmid1s) (writers cmid2s)
  let unsplit = teeIn (readers cmid2s) cout
  runParallel_ $ split : unsplit : procs

teeIn :: (Show a, Monoid a) => [Chanin a] -> Chanout a -> CHP ()
teeIn cins cout = go
  where
    go = do
      as <- (runParallel $ map readChannel cins) `onPoisonRethrow` (mapM_ poison cins)
      writeChannel cout $ mconcat as

count :: Show a => CountsUpdater -> Chanin a -> Chanout Counts -> CHP ()
count cu cin cout = go 0
  where
    go i = (readChannel cin >> go (i + 1))
           `onPoisonTrap` writeChannel cout (cu i)


-- |
-- | Splitting a file into parts
-- |

bytes' :: Chanin String -> Chanout Word8 -> CHP ()
bytes' = CHP.map (unpack . pack) <=> (CHP.stream :: Chanin [Word8] -> Chanout Word8 -> CHP ())

chars' :: Chanin String -> Chanout Char -> CHP ()
chars' = CHP.stream

words' :: Chanin String -> Chanout String -> CHP ()
words' = CHP.map words <=> (CHP.stream :: Chanin [String] -> Chanout String -> CHP ())

lines' :: Chanin String -> Chanout String -> CHP ()
lines' = CHP.map lines <=> (CHP.stream :: Chanin [String] -> Chanout String -> CHP ())
