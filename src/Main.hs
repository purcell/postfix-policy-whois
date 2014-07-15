{-
TODO: unified logging
TODO: timeouts on lookups with System.Timeout.timeout
TODO: command line args
TODO: keep patterns in a file
-}
module Main where
import           Cache
import           Control.Applicative        ((<$>))
import qualified Data.ByteString.Char8      as B8
import qualified Data.ByteString.Lazy.Char8 as BL8
import           Lookups
import           PostfixPolicy
import           System.Environment         (getArgs)
import           System.Posix.Syslog
import qualified Text.Regex.PCRE.Light      as PCRE
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


cachedWhoisLookup :: FilePath -> WhoisLookup
cachedWhoisLookup cacheDir domain = do
  dircache <- wrapGzip <$> bytestringDirectoryCache cacheDir
  withCache dircache unwrap wrap (whois . B8.pack) (B8.unpack domain)
  where
    unwrap (Right (WhoisInfo s)) = Just $ BL8.pack s
    unwrap _ = Nothing
    wrap s = Right $ WhoisInfo $ BL8.unpack s


main :: IO ()
main = do
  [port, datadir] <- getArgs
  withSyslog "policy-whois" [PID, PERROR] MAIL $
    case patterns of
      Left e -> syslog Error e
      Right ps -> serveTCP (read port :: Int)
                           logger
                           (whoisBlacklistPolicy ps logger (cachedWhoisLookup datadir))
  where logger = syslog Info
