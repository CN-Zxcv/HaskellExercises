--{-# LANGUAGE ScopedTypeVariables #-}

module Glob (namesMatching) where

import System.Directory
    ( doesDirectoryExist
    , doesFileExist
    , getCurrentDirectory
    , getDirectoryContents
    )

import System.FilePath
    ( dropTrailingPathSeparator
    , splitFileName
    , (</>)
    )

--import Control.Exception (handle, SomeException)
import Control.Exception (handle)
import Control.Monad (forM)
import GlobRegex (matchesGlob)

isPattern :: String -> Bool
isPattern = any (`elem` "[*?")

namesMatching :: String -> IO [String]
namesMatching pat
    -- plain path
    | not (isPattern pat) = do
        exists <- doesNameExist pat
        return (if exists then [pat] else [])
    -- regex path
    | otherwise = do
        case splitFileName pat of
            ("", baseName) -> do
                -- no path
                curDir <- getCurrentDirectory
                listMatches curDir baseName
            (dirName, baseName) -> do
                -- in path
                -- get all matching subdirs
                dirs <- if isPattern dirName
                    then namesMatching (dropTrailingPathSeparator dirName)
                    else return [dirName]
                -- select baseName match func
                let listDir = if isPattern baseName
                    then listMatches
                    else listPlain
                -- search all subdirs
                pathNames <- forM dirs $ \dir -> do
                    baseName <- listDir dir baseName
                    return (map (dir </>) baseName)
                return (concat pathNames)

doesNameExist :: FilePath -> IO Bool
doesNameExist name = do
    fileExists <- doesFileExist name
    directoryExists <- doesDirectoryExist name
    return (fileExists || directoryExists)

listMatches :: FilePath -> String -> IO [String]
listMatches dirName pat = do
    dirName' <- if null dirName
        then getCurrentDirectory
        else return dirName
    --handle (\(e::SomeException) -> return []) $ do
    handle ((const (return [])) :: IOError -> IO [String]) $ do
        names <- getDirectoryContents dirName'
        let names' = if isHidden pat
            then filter isHidden names
            else filter (not . isHidden) names
        return (filter (`matchesGlob` pat) names')

isHidden ('.':_) = True
isHidden _ = False

listPlain :: FilePath -> String -> IO [String]
listPlain dirName baseName = do
    exists <- if null baseName
        then doesDirectoryExist dirName
        else doesNameExist (dirName </> baseName)
    return (if exists then [baseName] else [])
