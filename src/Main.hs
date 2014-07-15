{-
TODO: unified logging
TODO: timeouts on lookups with System.Timeout.timeout
TODO: cache lookups
TODO: command line args
TODO: keep patterns in a file
-}
module Main where
import qualified Data.ByteString.Char8 as B8
import           Lookups
import           Network.URI           (escapeURIString)
import           PostfixPolicy
import           System.Directory      (doesFileExist)
import           System.Environment    (getArgs)
import           System.FilePath
import           System.Posix.Syslog
import qualified Text.Regex.PCRE.Light as PCRE
import           WhoisPolicy



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



cachedWhoisLookup :: FilePath -> Logger -> WhoisLookup
cachedWhoisLookup cacheDir logger = cachedlookup
  where
    cachedlookup :: WhoisLookup
    cachedlookup domain = do
      haveCache <- doesFileExist cacheFilename
      if haveCache
      then do
        logger $ "Cache hit for " ++ B8.unpack domain
        (Right . WhoisInfo) `fmap` readFile cacheFilename
      else do
        logger $ "Cache miss for " ++ B8.unpack domain
        result <- whois domain
        case result of
          Right info -> do
            cacheIt info
            return result
          _ -> return result
      where
        cacheFilename = cacheDir </> escapeDomain (B8.unpack domain)
        escapeDomain = escapeURIString ('/' /=)
        cacheIt (WhoisInfo s) = writeFile cacheFilename s


main :: IO ()
main = do
  [port, datadir] <- getArgs
  withSyslog "policy-whois" [PID, PERROR] MAIL $
    case patterns of
      Left e -> syslog Error e
      Right ps -> serveTCP (read port :: Int)
                           logger
                           (whoisBlacklistPolicy ps logger (cachedWhoisLookup datadir logger))
  where logger = syslog Info
