module Main where

import Control.Concurrent.CHP
import qualified Control.Concurrent.CHP.Common as CHP
import Control.Concurrent.CHP.Connect
import Control.Concurrent.CHP.Console

produce :: String -> Chanout String -> CHP ()
produce s c = writeChannel c s

receive :: Chanin String -> CHP ()
receive c = readChannel c >>= liftIO_CHP . putStrLn

--process :: ConsoleChans -> CHP ()
proc = produce "abcdef" |<=>| receive

main = runCHP_ proc