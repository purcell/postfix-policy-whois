{-# LANGUAGE MultiParamTypeClasses #-}
module Cache
       ( Cache(..)
       , withCache
       , stringDirectoryCache
       )
where
import           Control.Applicative ((<$>))
import           Network.URI         (escapeURIString)
import           System.Directory    (doesFileExist)
import           System.FilePath



data Cache a b = Cache { cWrite :: a -> b -> IO ()
                       , cRead  :: a -> IO (Maybe b)
                       }


withCache :: Cache a b -> (c -> Maybe b) -> (b -> c) -> (a -> IO c) -> a -> IO c
withCache cache unwrap wrap f key = do
  found <- cRead cache key
  case found of
    Just result -> return $ wrap result
    Nothing -> do
      result <- f key
      case unwrap result of
        Just v -> cWrite cache key v
        _ -> return ()
      return result


stringDirectoryCache :: FilePath -> IO (Cache String String)
stringDirectoryCache cacheDir = return $ Cache writer reader
  where
    writer key = writeFile (cacheFilename key)
    reader key = do
      haveCache <- doesFileExist (cacheFilename key)
      if haveCache
        then Just <$> readFile (cacheFilename key)
        else return Nothing
    cacheFilename key = cacheDir </> escapeKey key
    escapeKey = escapeURIString ('/' /=)

