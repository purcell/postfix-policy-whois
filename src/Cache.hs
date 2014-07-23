{-# LANGUAGE MultiParamTypeClasses #-}
module Cache
       ( Cache(..)
       , withCache
       , bytestringDirectoryCache
       , wrapGzip
       )
where
import           Codec.Compression.Zlib.Internal (compress, decompress,
                                                  defaultCompressParams,
                                                  defaultDecompressParams,
                                                  gzipFormat)
import           Control.Applicative             ((<$>))
import qualified Data.ByteString.Lazy.Char8      as BL8
import           Data.Hashable
import           Network.URI                     (escapeURIString)
import           Numeric                         (showHex)
import           System.Directory                (createDirectoryIfMissing,
                                                  doesFileExist)
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


wrapGzip :: Cache String BL8.ByteString -> Cache String BL8.ByteString
wrapGzip underlying = Cache wrapWriter wrapReader
  where
    wrapWriter k v = cWrite underlying k (squish v)
    squish = compress gzipFormat defaultCompressParams
    wrapReader k = do
      result <- cRead underlying k
      return $ unsquish <$> result
    unsquish = decompress gzipFormat defaultDecompressParams


bytestringDirectoryCache :: FilePath -> IO (Cache String BL8.ByteString)
bytestringDirectoryCache cacheDir = return $ Cache writer reader
  where
    writer key val = do
      createDirectoryIfMissing True $ takeDirectory fn
      BL8.writeFile fn val
      where fn = cacheFilename key
    reader key = do
      haveCache <- doesFileExist (cacheFilename key)
      if haveCache
        then Just <$> BL8.readFile (cacheFilename key)
        else return Nothing
    cacheFilename key = cacheDir </> hashPrefix key </> escapeKey key
    escapeKey = escapeURIString ('/' /=)
    hashPrefix key = take 2 hex </> take 4 hex
      where hex = showHex (hash key) ""
