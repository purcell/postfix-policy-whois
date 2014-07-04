{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-
TODO: IOExceptions, SIGPIPE
-}
module Main where

import           Control.Applicative ((<$>), (<*))
import           Control.Exception   (IOException, bracket, catch)
import           Control.Monad       (forever)
import qualified Data.Map.Strict     as Map
import           Lookups
import qualified Network
import qualified Network.Socket      as Socket
import           System.IO           (Handle, hClose, hGetLine, hPutStr)
import           Text.Parsec.Char
import           Text.Parsec.Error   (ParseError)
import           Text.Parsec.Prim


type PolicyInfo = Map.Map String String

data Action = Reject | Dunno | Tempfail
            deriving Show

-- | Policies take info about the incoming mail and return a decision
-- about what, if anything, Postfix should do with it.
type Policy = PolicyInfo -> IO Action



many1 :: ParsecT s u m a -> ParsecT s u m [a]
many1 p = do
  x <- p
  xs <- many p
  return $ x:xs

parsePolicyInfo :: Stream s m Char => ParsecT s u m PolicyInfo
parsePolicyInfo = Map.fromList <$> many1 policyLine
  where
    policyLine = do
      k <- many1 (alphaNum <|> char '_') <* char '='
      v <- many (noneOf "\n") <* newline
      return (k, v)


type Logger = String -> IO ()


actionToResponse :: Action -> String
actionToResponse a = "action=" ++ code ++ "\n\n"
  where code = case a of
                 Reject -> "reject"
                 Dunno -> "dunno"
                 Tempfail -> "tempfail"


runHandle :: Handle -> Logger -> Policy -> IO ()
runHandle handle logger policy = do
  action <- readAndDecide `catch` logErrorAndReturn Dunno
  logger $ "Policy decision: " ++ show action
  hPutStr handle (actionToResponse action) `catch` logErrorAndReturn ()
  where
    logErrorAndReturn v e = do
      logger $ "Error: " ++ show (e :: IOException)
      return v
    readAndDecide = do
      input <- readLinesUntilBlank
      case parse parsePolicyInfo "" input of
        Left err -> do
          logger $ "Error parsing request: " ++ show err
          return Dunno
        Right req -> policy req
    readLinesUntilBlank = acc ""
        where acc ls = do
                line <- hGetLine handle
                if null line
                then return ls
                else acc $ ls ++ line ++ "\n"


serveTCP :: Int -> Logger -> Policy -> IO ()
serveTCP port logger policy = Network.withSocketsDo $ bracket listen Socket.close (forever . accept)
  where
    listen = do
      s <- Network.listenOn $ Network.PortNumber $ fromIntegral port
      logger $ "Listening on port " ++ show port
      return s
    accept sock = bracket (Network.accept sock) (\(h, _, _) -> hClose h) handler
    handler (handle, chost, cport) = do
      logger $ "Connection from " ++ chost ++ ":" ++ show cport
      runHandle handle logger policy


main :: IO ()
main = undefined
