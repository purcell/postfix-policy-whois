{-
TODO: IOExceptions
-}
module Main where

import           Lookups

data Action = Reject | Dunno | Tempfail
            deriving Show

main :: IO ()
main = undefined
