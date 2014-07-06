module Main where
import           PostfixPolicy

main :: IO ()
main = serveTCP 2020 putStrLn (\_ -> return (Decision Dunno))
