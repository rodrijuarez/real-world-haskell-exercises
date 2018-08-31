module BetterPredicate where

import Control.Exception (bracket, handle)
import Control.Monad (filterM)
import ControlledVisit (Info)
import Data.Time (UTCTime)
import System.Directory
       (Permissions(..), getModificationTime, getPermissions)
import System.FilePath (takeExtension)
import System.IO (IOMode(..), hClose, hFileSize, openFile)

import RecursiveContents (getRecursiveContents)

getFileSize :: FilePath -> IO (Maybe Integer)
getFileSize path =
  handle ((\_ -> return Nothing) :: IOError -> IO (Maybe Integer)) $
  bracket (openFile path ReadMode) hClose $ \h -> do
    size <- hFileSize h
    return (Just size)

betterFind :: InfoP Bool -> FilePath -> IO [FilePath]
betterFind p path = getRecursiveContents path >>= filterM check
  where
    check name = do
      perms <- getPermissions name
      size <- getFileSize name
      modified <- getModificationTime name
      return (p name perms size modified)

myTest4 = liftPath takeExtension ==? ".cpp" &&? sizeP >? 131072

liftPath :: (FilePath -> a) -> InfoP a
liftPath f w _ _ _ = f w

type InfoP a = FilePath -> Permissions -> Maybe Integer -> UTCTime -> a

pathP :: InfoP FilePath
pathP path _ _ _ = path

sizeP :: InfoP Integer
sizeP _ _ (Just size) _ = size
sizeP _ _ Nothing _ = -1

liftP :: (a -> b -> c) -> InfoP a -> b -> InfoP c
liftP q f k w x y z = f w x y z `q` k

equalP :: (Eq a) => InfoP a -> a -> InfoP Bool
equalP f k = \w x y z -> f w x y z == k

greaterP, lesserP :: (Ord a) => InfoP a -> a -> InfoP Bool
greaterP = liftP (>)

lesserP = liftP (<)

liftP2 :: (a -> b -> c) -> InfoP a -> InfoP b -> InfoP c
liftP2 q f g w x y z = f w x y z `q` g w x y z

andP = liftP2 (&&)

orP = liftP2 (||)

constP :: a -> InfoP a
constP k _ _ _ _ = k

infix 4 ==?

infixr 3 &&?

infix 4 >?

(==?) = equalP

(&&?) = andP

(>?) = greaterP

liftP' q f k w x y z = f w x y z `q` constP k w x y z
