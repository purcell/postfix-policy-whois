{-
TODO: stylish
-}
module Main where

import Network.DNS.Lookup (lookupNS)
import Network.DNS.Resolver (defaultResolvConf, makeResolvSeed, withResolver)
import Network.DNS.Types (Domain)
import qualified Data.ByteString.Char8 as B8 hiding (intersperse)
import Data.List (intersperse)
import Data.Maybe (catMaybes)
import Network.Whois as Whois
import Control.Applicative ((<$>))
import Control.Monad.Trans.Error

data Action = Reject | Dunno | Tempfail
            deriving Show


data DomainInfo = DomainInfo { dTLD :: Domain
                             , dNameServers :: [Domain] }
                deriving Show

-- | Given a dotted domain name, find its parent by chopping off the
-- first subdomain.
parentDomain :: Domain -> Maybe Domain
parentDomain d =
  case B8.unpack chopped of
    "." -> Nothing
    "" -> Nothing
    _ -> Just chopped
  where chopped = B8.concat $ intersperse (B8.pack ".") $ Prelude.drop 1 $ B8.split '.' d

-- | Find the nameservers for the given domain, or a Left error if
-- none are found.
nameserversFor :: Domain -> IO (Either String [Domain])
nameserversFor d = do
  seed <- makeResolvSeed defaultResolvConf
  ns <- withResolver seed $ \r -> lookupNS r d
  return $ case ns of
    Left err   -> Left (show err)
    Right []   -> Left "No nameservers found"
    Right svrs -> Right svrs

domainInfo :: Domain -> IO (Either String DomainInfo)
domainInfo name = do
  ns <- nameserversFor name
  case ns of
    Right n -> return $ Right (DomainInfo name n)
    Left e -> let parent = parentDomain name in
      case parent of
        Just p -> domainInfo p
        _ -> return $ Left e

data WhoisInfo = WhoisInfo String
               deriving Show

whoisForDomain :: Domain -> IO (Either String WhoisInfo)
whoisForDomain d = do
    (primary, secondary) <- whoisLookup d
    return $ case (primary, secondary) of
      (Nothing, Nothing) -> Left "No whois info found"
      _ -> Right $ WhoisInfo $ concat $ catMaybes [primary, secondary]
  where
    whoisLookup = Whois.whois . B8.unpack . trimTrailingDot
    trimTrailingDot :: Domain -> Domain
    trimTrailingDot bs = if B8.pack "." `B8.isSuffixOf` bs
                         then B8.init bs
                         else bs





main :: IO ()
main = undefined
