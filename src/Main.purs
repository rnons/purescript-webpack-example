module Main (main) where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)

greet :: String -> String
greet name = "Hello, " <> name <> "!"

main :: Eff (console :: CONSOLE) Unit
main = log (greet "World")
