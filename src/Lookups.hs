-- | Network lookups

module Lookups ( WhoisInfo(..)
               , whois
               , DomainInfo(..)
               , domainInfo
               )
where
import           Control.Applicative   ((<$>))
import           Control.Monad.Except
import qualified Data.ByteString.Char8 as B8 hiding (intersperse)
import           Data.List             (intersperse)
import           Data.Maybe            (catMaybes)
import           Network.DNS.Lookup    (lookupNS)
import           Network.DNS.Resolver  (defaultResolvConf, makeResolvSeed,
                                        withResolver)
import           Network.DNS.Types     (Domain)
import qualified Network.Whois         as Whois


data DomainInfo = DomainInfo { dTLD         :: Domain
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


type Lookup = ExceptT String IO

-- | Find the nameservers for the given domain, or a Left error if
-- none are found.
nameserversFor :: Domain -> Lookup [Domain]
nameserversFor d = do
  seed <- liftIO $ makeResolvSeed defaultResolvConf
  ns <- withExceptT show $ ExceptT $ withResolver seed $ \r -> lookupNS r d
  if null ns
  then throwError "No nameservers found"
  else return ns

domainInfo :: Domain -> Lookup DomainInfo
domainInfo name = (DomainInfo name <$> nameserversFor name)
                  `catchError` \e ->
                      case parentDomain name of
                        Just p -> domainInfo p
                        _ -> throwError e

data WhoisInfo = WhoisInfo String
               deriving Show

whoisForDomain :: Domain -> Lookup WhoisInfo
whoisForDomain d = do
  (primary, secondary) <- liftIO $ whoisLookup d
  case (primary, secondary) of
    (Nothing, Nothing) -> throwError "No whois info found"
    _ -> return $ WhoisInfo $ concat $ catMaybes [primary, secondary]
  where
    whoisLookup = Whois.whois . B8.unpack . trimTrailingDot
    trimTrailingDot :: Domain -> Domain
    trimTrailingDot bs = if B8.pack "." `B8.isSuffixOf` bs
                         then B8.init bs
                         else bs


whois :: String -> IO (Either String WhoisInfo)
whois domain = runExceptT $ domainInfo (B8.pack domain) >>= (whoisForDomain . dTLD)
