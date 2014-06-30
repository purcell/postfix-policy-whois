{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-
TODO: IOExceptions
-}
module Main where

import           Control.Applicative ((<*))
import           Control.Exception   (bracket)
import           Control.Monad       (forever)
import qualified Data.Map.Strict     as Map
import           Lookups
import qualified Network
import qualified Network.Socket      as Socket
import           System.IO           (Handle, hClose, hGetLine)
import           Text.Parsec.Char
import           Text.Parsec.Error   (ParseError)
import           Text.Parsec.Prim

data Action = Reject | Dunno | Tempfail
            deriving Show



type PolicyInfo = Map.Map String String

parsePolicyLine :: Stream s m Char => ParsecT s u m (String, String)
parsePolicyLine = do
  k <- many (alphaNum <|> char '_') <* char '='
  v <- many (noneOf "\n") <* newline
  return (k, v)

parsePolicyInfo :: Stream s m Char => ParsecT s u m PolicyInfo
parsePolicyInfo = do
  l <- parsePolicyLine
  ls <- many parsePolicyLine
  return $ Map.fromList (l:ls)


debugOut :: String -> IO ()
debugOut = putStrLn

serve :: Int -> IO ()
serve port = Network.withSocketsDo $ bracket listen Socket.close (forever . accept)
  where
    listen = do
      s <- Network.listenOn $ Network.PortNumber $ fromIntegral port
      debugOut $ "Listening on port " ++ show port
      return s
    accept sock = bracket (Network.accept sock) (\(h, _, _) -> hClose h) handler
    handler (handle, chost, cport) = do
      debugOut $ "Connection from " ++ chost ++ ":" ++ show cport
      input <- readLinesUntilBlank handle
      let info = parse parsePolicyInfo "" input in
        print info
    readLinesUntilBlank :: Handle -> IO String
    readLinesUntilBlank h = acc ""
        where acc ls = do
                line <- hGetLine h -- TODO: handle end of file
                if null line
                then return ls
                else acc $ ls ++ line ++ "\n"


main :: IO ()
main = undefined
