{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-
TODO: SIGPIPE
TODO: More response types, allow response string
TODO: Policy info fields as a sum type
TODO: Separate Types.hs

http://www.postfix.org/SMTPD_POLICY_README.html

-}
module PostfixPolicy
       ( Action(..)
       , Decision(..)
       , PolicyInfo
       , Policy
       , inconclusive
       , Logger
       , runHandle
       , serveTCP
       )
where
import           Control.Applicative ((<$>), (<*))
import           Control.Exception   (IOException, bracket, catch)
import           Control.Monad       (forever, liftM2)
import qualified Data.Map.Strict     as Map
import qualified Network
import qualified Network.Socket      as Socket
import           System.IO           (Handle, hClose, hGetLine, hPutStr)
import           Text.Parsec.Char
import           Text.Parsec.Prim
import           Text.Parsec.String  ()


-- | A map of key/value pairs as described in
-- http://www.postfix.org/SMTPD_POLICY_README.html
type PolicyInfo = Map.Map String String

-- | A response action for Postfix, as listed in
-- http://www.postfix.org/access.5.html
-- Only a subset of those are made available here.
data Action = Reject | Dunno | Tempfail

instance Show Action where
  show Reject = "reject"
  show Dunno = "dunno"
  show Tempfail = "tempfail"

data Decision = Decision Action
              | DecisionWithMessage Action String

instance Show Decision where
  show (Decision a) = show a
  show (DecisionWithMessage a s) = show a ++ " " ++ s

inconclusive :: Decision
inconclusive = Decision Dunno


-- | Policies take info about the incoming mail and return a decision
-- about what, if anything, Postfix should do with it.
type Policy = PolicyInfo -> IO Decision

type Logger = String -> IO ()


many1 :: ParsecT s u m a -> ParsecT s u m [a]
many1 p = liftM2 (:) p (many p)

parsePolicyInfo :: Stream s m Char => ParsecT s u m PolicyInfo
parsePolicyInfo = Map.fromList <$> many1 policyLine
  where
    policyLine = do
      k <- many1 (alphaNum <|> char '_') <* char '='
      v <- many (noneOf "\n") <* newline
      return (k, v)


decisionToResponse :: Decision -> String
decisionToResponse d = "action=" ++ show d ++ "\n\n"


runHandle :: Handle -> Logger -> Policy -> IO ()
runHandle handle logger policy = do
  decision <- readAndDecide `catch` \e -> do logError e; return inconclusive
  logger $ "Policy decision: " ++ show decision
  hPutStr handle (decisionToResponse decision) `catch` logError
  where
    logError e = logger $ "Error: " ++ show (e :: IOException)
    readAndDecide = do
      input <- readLinesUntilBlank
      case parse parsePolicyInfo "" input of
        Left err -> do
          logger $ "Error parsing request: " ++ show err
          return inconclusive
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

