module Glob where

import Control.Exception (handle)
import System.Directory
       (doesDirectoryExist, doesFileExist, getCurrentDirectory,
        getDirectoryContents)
import System.FilePath
       ((</>), dropTrailingPathSeparator, isSearchPathSeparator,
        splitFileName)

import Control.Monad (forM)
import GlobRegex (matchesGlob)

isPattern :: String -> Bool
isPattern = any (`elem` "[*?")

namesMatching pat
  | not (isPattern pat) = do
    exists <- doesNameExist pat
    return $ if' exists [pat] []
  | otherwise = do
    case splitFileName pat of
      ("", baseName) -> do
        curDir <- getCurrentDirectory
        listMatches curDir baseName
      (dirName, baseName) -> do
        dirs <-
          if isPattern dirName
            then namesMatching (dropTrailingPathSeparator dirName)
            else return [dirName]
        let listDir =
              if isPattern baseName
                then listMatches
                else listPlain
        pathNames <-
          forM dirs $ \dir -> do
            baseNames <- listDir dir baseName
            return (map (dir </>) baseNames)
        return (concat pathNames)

doesNameExist :: FilePath -> IO Bool
doesNameExist name = do
  fileExists <- doesFileExist name
  if' fileExists (return True) (doesDirectoryExist name)

listPlain :: FilePath -> String -> IO [String]
listPlain dirName baseName = do
  exists <-
    if null baseName
      then doesDirectoryExist dirName
      else doesNameExist (dirName </> baseName)
  return
    (if exists
       then [baseName]
       else [])

(|>) :: a -> (a -> b) -> b
(|>) x f = f x

if' :: Bool -> a -> a -> a
if' True x _ = x
if' False _ y = y

listMatches :: FilePath -> String -> IO [String]
listMatches dirName pattern = do
  dirName' <- if' (null dirName) getCurrentDirectory (return dirName)
  handle ((const (return [])) :: IOError -> IO [String]) $ do
    names <- getDirectoryContents dirName'
    filterHidden (isHidden pattern) names |> (filter (`matchesGlob` pattern)) |>
      return

filterHidden :: Bool -> [String] -> [String]
filterHidden showHidden names =
  if showHidden
    then filter isHidden names
    else filter (not . isHidden) names

isHidden ('.':_) = True
isHidden _ = False
