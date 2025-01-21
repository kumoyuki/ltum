module REPL where

import Control.Monad.Trans
import Control.Monad.Catch
import Prelude hiding (catch)

import System.Console.Haskeline as HL

main :: IO ()
main = HL.runInputT HL.defaultSettings mloop

mloop = do
  buffer <- HL.getInputLine "hello sailor."
  case buffer of
    (Just answer) -> do
       outputStrLn ("Wow. Really? \"" ++ answer ++ "\"?")
       mloop
    Nothing -> outputStrLn "Be that way..."



