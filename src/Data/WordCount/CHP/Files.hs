{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

module Data.WordCount.CHP.Files (
) where

import Prelude (($), (.), flip, (+), (++), (>), map, IO, Show(..), Eq(..), String, FilePath)
import Data.Monoid (Monoid(..))
import Data.Maybe (Maybe(..))
import System.IO (readFile, getContents, FilePath, putStrLn)
import Control.Monad (Monad(..), mapM_, forever)

import Data.WordCount.Types (Counts(..), addCounts, Counter(..), printCounts, summarize)
import Data.WordCount.CHP.Core (CHPCounter, runCounter)

import Control.Concurrent.CHP
import qualified Control.Concurrent.CHP.Common as CHP
import Control.Concurrent.CHP.Connect

import Control.Concurrent.CHP.Traces


data FileStream = File FilePath | StdIn | End
  deriving (Eq, Show)

instance Counter CHPCounter where
  countFiles = wcFiles
  countStdin = wcStdin

wcFiles :: [FilePath] -> CHPCounter -> IO ()
wcFiles fps cntr = runFileStream (map File fps) cntr

wcStdin :: CHPCounter -> IO ()
wcStdin cntr = runFileStream [StdIn] cntr

runFileStream :: [FileStream] -> CHPCounter -> IO ()
runFileStream fss cntr = runCHP_ $ (produce fss |<=> fileCounts cntr |<=> collect |<=> printem |<=>| consume) `onPoisonTrap` return ()
  where consume = CHP.consume :: Chanin (FileStream, Counts) -> CHP ()

  ----

produce :: [FileStream] -> Chanout FileStream -> CHP ()
produce as c = mapM_ (writeChannel c) (as ++ [End]) >> poison c

fileCounts :: CHPCounter -> Chanin FileStream -> Chanout (FileStream, Counts) -> CHP ()
fileCounts cc = maptuple go
  where
    go cin cout = readChannel cin >>= f cout >> go cin cout
    f cout (File fp) = do
      s <- liftIO_CHP $ readFile fp
      runCounter cc s cout
    f cout StdIn = do
      s <- liftIO_CHP getContents
      runCounter cc s cout
    f cout End = writeChannel cout mempty

collect :: Chanin (FileStream, Counts) -> Chanout (FileStream, Counts) -> CHP ()
collect = stateful f mempty
  where
    f (End, _) master = ((End, master), master)
    f (fs, cnts) master = ((fs, cnts), master `addCounts` cnts)

printem :: Chanin (FileStream, Counts) -> Chanout (FileStream, Counts) -> CHP ()
printem = mapCHP_ f
  where
    f (File fp,cnts) = liftIO_CHP (printCounts cnts fp)
    f (StdIn,cnts) = liftIO_CHP (printCounts cnts "")
    f (End,cnts) = liftIO_CHP (summarize cnts)


-- | A stateful stream (a :: input, b :: output, c :: state)
stateful :: (a -> c -> (b,c)) -> c -> Chanin a -> Chanout b -> CHP ()
stateful f init_ cin cout = go init_ `onPoisonRethrow` poison cout >> poison cin
  where
    go s = do
      a <- readChannel cin
      let (b,t) = f a s
      writeChannel cout b
      go t

  -- | A mapping stream that does CHP but just passes through
mapCHP_ :: (a -> CHP ()) -> Chanin a -> Chanout a -> CHP ()
mapCHP_ f cin cout = go `onPoisonRethrow` poison cout >> poison cin
  where go = readChannel cin >>= (\x -> f x >> writeChannel cout x >> go)

  -- | A mapping stream that does CHP
mapCHP :: (a -> Chanout b -> CHP ()) -> Chanin a -> Chanout b -> CHP ()
mapCHP f cin cout = forever (readChannel cin >>= flip f cout)
  `onPoisonRethrow` (poison cout >> poison cin)

-- | perform a mapping operation but join the original value into a tuple with the mapped one
maptuple :: Show a => (Chanin a -> Chanout b -> CHP ()) -> Chanin a -> Chanout (a, b) -> CHP ()
maptuple proc cin cout = do
    [c1, c2] <- newChannelListWithLabels ["maptuple-1", "maptuple-2"]
    [c3] <- newChannelListWithLabels ["maptuple-3"]
    let tee = (CHP.parDelta cin $ writers [c1, c2])         `onPoisonRethrow` mapM_ poison (writers [c1, c2])
    let prc = (proc (reader c1) (writer c3))                `onPoisonRethrow` poison (writer c3)
    let join = (CHP.join (,) (reader c2) (reader c3) cout)  `onPoisonRethrow` poison cout
    tee <|*|> prc <|*|> join

