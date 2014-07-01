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


debugOut :: String -> IO ()
debugOut = putStrLn


actionToResponse :: Action -> String
actionToResponse a = "action=" ++ code ++ "\n\n"
  where code = case a of
                 Reject -> "reject"
                 Dunno -> "dunno"
                 Tempfail -> "tempfail"


serve :: Int -> Policy -> IO ()
serve port policy = Network.withSocketsDo $ bracket listen Socket.close (forever . accept)
  where
    listen = do
      s <- Network.listenOn $ Network.PortNumber $ fromIntegral port
      debugOut $ "Listening on port " ++ show port
      return s
    accept sock = bracket (Network.accept sock) (\(h, _, _) -> hClose h) handler
    handler (handle, chost, cport) = do
      debugOut $ "Connection from " ++ chost ++ ":" ++ show cport
      action <- readAndDecide handle `catch` logErrorAndReturn Dunno
      debugOut $ "Policy decision: " ++ show action
      hPutStr handle (actionToResponse action) `catch` logErrorAndReturn ()
    logErrorAndReturn v e = do
      debugOut $ "Error: " ++ show (e :: IOException)
      return v
    readAndDecide handle = do
      input <- readLinesUntilBlank handle
      case parse parsePolicyInfo "" input of
        Left err -> do
          debugOut $ "Error parsing request: " ++ show err
          return Dunno
        Right req -> policy req
    readLinesUntilBlank :: Handle -> IO String
    readLinesUntilBlank h = acc ""
        where acc ls = do
                line <- hGetLine h
                if null line
                then return ls
                else acc $ ls ++ line ++ "\n"


main :: IO ()
main = undefined
