module BetterPredicate where

import Control.Exception (bracket, handle)
import Control.Monad (filterM)
import Data.Time (UTCTime)
import System.Directory
       (Permissions(..), getModificationTime, getPermissions)
import System.FilePath (takeExtension)
import System.IO (IOMode(..), hClose, hFileSize, openFile)

import RecursiveContents (getRecursiveContents)

type Predicate = FilePath -> Permissions -> Maybe Integer -> UTCTime -> Bool

getFileSize :: FilePath -> IO (Maybe Integer)
getFileSize path =
  handle ((\_ -> return Nothing) :: IOError -> IO (Maybe Integer)) $
  bracket (openFile path ReadMode) hClose $ \h -> do
    size <- hFileSize h
    return (Just size)
  --h <- openFile path ReadMode
  --size <- hFileSize h
  --hClose h
  --return size
  --handle (\_ -> return Nothing) $ do
    --h <- openFile path ReadMode
    --size <- hFileSize h
    --hClose h
    --return (Just size)

--simpleFileSize :: FilePath -> IO Integer
--simpleFileSize path = do
--saferFileSize :: FilePath -> IO (Maybe Integer)
--saferFileSize path =
betterFind :: Predicate -> FilePath -> IO [FilePath]
betterFind p path = getRecursiveContents path >>= filterM check
  where
    check name = do
      perms <- getPermissions name
      size <- getFileSize name
      modified <- getModificationTime name
      return (p name perms size modified)
