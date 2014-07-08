{-
TODO: unified logging
TODO: timeouts on lookups with System.Timeout.timeout
TODO: cache lookups
TODO: command line args
TODO: keep patterns in a file
-}
module Main where
import qualified Data.ByteString.Char8 as B8
import           PostfixPolicy
import           System.Environment    (getArgs)
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

main :: IO ()
main = do
  [port] <- getArgs
  withSyslog "policy-whois" [PID, PERROR] MAIL $
    case patterns of
      Left e -> syslog Error e
      Right ps -> serveTCP (read port :: Int) (syslog Info) (whoisBlacklistPolicy ps (syslog Info))
