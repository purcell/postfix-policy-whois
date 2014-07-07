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
            "whoisguard\\.com"
           ]
  where
    compile :: String -> Either String PCRE.Regex
    compile s = PCRE.compileM (B8.pack s) [PCRE.caseless]


decideBasedOnWhois :: [PCRE.Regex] -> WhoisInfo -> IO Decision
decideBasedOnWhois matchers (WhoisInfo info) =
  let binfo = B8.pack info in
  if null $ mapMaybe (\p -> PCRE.match p binfo []) matchers
  then return $ Decision Dunno
  else do
    putStrLn "Rejecting based on pattern match"
    return $ Decision Reject


hardcodedPolicy :: [PCRE.Regex] -> Policy
hardcodedPolicy matchers info = do
  domain <- return . runIdentity $ runExceptT $ senderDomain info
  case domain of
    Left e -> bail e
    Right d -> do
      whoisInfo <- whois d
      case whoisInfo of
        Left e -> bail e
        Right winfo -> decideBasedOnWhois matchers winfo
  where bail e = do putStrLn e; return $ Decision Dunno


main :: IO ()
main = do
  [port] <- getArgs
  case patterns of
    Left e -> putStrLn e
    Right ps -> serveTCP (read port :: Int) putStrLn (hardcodedPolicy ps)
