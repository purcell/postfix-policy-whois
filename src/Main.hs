{-
TODO: unified logging
TODO: timeouts on lookups with System.Timeout.timeout
TODO: cache lookups
TODO: command line args
TODO: keep patterns in a file
-}
module Main where
import           Control.Applicative    ((*>))
import           Control.Monad.Except
import           Control.Monad.Identity
import qualified Data.ByteString.Char8  as B8
import qualified Data.Map               as Map
import           Data.Maybe             (mapMaybe)
import           Lookups
import           PostfixPolicy
import           System.Environment     (getArgs)
import           Text.Parsec.Char
import           Text.Parsec.Prim
import qualified Text.Regex.PCRE.Light  as PCRE

domainFromEmail :: String -> ExceptT String Identity Domain
domainFromEmail email = withExcept translateError . ExceptT . Identity $ parseEmail email
  where
    translateError _ = "Invalid email address: " ++ show email
    parseEmail = fmap B8.pack . parse (notAt *> char '@' *> notAt) ""
    notAt = many1 $ noneOf "@"
    many1 p = liftM2 (:) p (many p)


senderDomain :: PolicyInfo -> ExceptT String Identity Domain
senderDomain info = do
  email <- case Map.lookup "sender" info of
             Just s -> return s
             Nothing -> throwError "No sender provided"
  domainFromEmail email


patterns :: Either String [PCRE.Regex]
patterns = mapM compile [
            "EXPIRED - PENDING DELETE",
            "ADDPERIOD",
            "monikerprivacy",
            "whoisprivacyprotect\\.com",
            "whoisguard"
           ]
  where
    compile s = PCRE.compileM (B8.pack s) [PCRE.caseless]


decideBasedOnWhois :: [PCRE.Regex] -> WhoisInfo -> IO Decision
decideBasedOnWhois matchers (WhoisInfo info) =
  let binfo = B8.pack info in
  if null $ mapMaybe (\p -> PCRE.match p binfo []) matchers
  then return $ Decision Dunno
  else do
    putStrLn "Rejecting based on pattern match"
    return $ Decision Reject


blacklistPolicy :: [PCRE.Regex] -> Policy
blacklistPolicy matchers info = do
  result <- runExceptT lookupAndDecide
  case result of
    Left e -> do
      putStrLn e
      return $ Decision Dunno
    Right d -> return d
  where
    lookupAndDecide = do
      domain <- mapExceptT (return . runIdentity) $ senderDomain info
      winfo <- ExceptT $ whois domain
      liftIO $ decideBasedOnWhois matchers winfo


main :: IO ()
main = do
  [port] <- getArgs
  case patterns of
    Left e -> putStrLn e
    Right ps -> serveTCP (read port :: Int) putStrLn (blacklistPolicy ps)
