module REPL where

import Control.Monad.Trans
import Control.Monad.Catch
import Prelude hiding (catch)

import System.Console.Haskeline as HL


-- one of the points of this code is to figure out WTF the type should be
-- for EVAL in a lambda calculus with macros. Remember that the existence
-- of "define" pretty much requires that EVAL implement a State monad, so
-- what does it even mean to have a LC w/macros?

-- more specifically, how do we encode an LC interpreter in a minimalist
-- LC? Note that we are actually leaning into Scheme as our model (b/c the
-- paper), so we may want to allow certain syntactic sugars like letrec
-- 

data Datum = Nil | Cons Datum Datum | Symbol String

data PD = Exit | Malformed String | SExp Datum


-- expand later
parse :: Maybe String -> PD
parse Nothing = Exit
parse (Just buffer) = SExp Nil

--render :: Datum -> IO ()
render d = do
  outputStrLn "rendered"

-- main IO loop
main :: IO ()
main = HL.runInputT HL.defaultSettings mloop

mloop = do
  buffer <- HL.getInputLine "hello sailor."
  pdatum <- parse buffer
  case pdatum of
    (SExp datum) -> do
      render datum
      mloop

    (Malformed text) -> do
      outputStrLn ("Wow. Really? \"" ++ text ++ "\"?")
      mloop
                
    Exit -> outputStrLn "Be that way..."



