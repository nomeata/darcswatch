{-
Copyright (C) 2008 Joachim Breitner

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; see the file COPYING.  If not, write to
the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
Boston, MA 02110-1301, USA.
-}

module Darcs.Watch.Storage where

import Darcs
import Darcs.Watch.Data

import Data.Time
import qualified Data.ByteString.Char8 as B
import qualified Data.Map as M
import Data.ByteString.Char8 (ByteString)
import System.FilePath
import System.Directory
import Data.List
import Control.Applicative
import System.Directory
import Control.Monad
import Data.Char


-- | Adds a new patch bundle to the stogare
addBundle :: StorageConf -> PatchBundle -> IO BundleHash
addBundle path bundle = do
	let hash = hash_bundle bundle
	let dataFile = bundleDir path </> hash <.> "data"
	ex <- doesFileExist dataFile
	unless ex $ B.writeFile dataFile (B.pack "[]")
	B.writeFile (bundleFileName path hash)
	            (make_bundle bundle)
	return hash


-- | Retrieves a new patch bundle from the storage
getBundle :: StorageConf -> BundleHash -> IO PatchBundle
getBundle path hash = 
	either error id . scan_bundle <$> B.readFile (bundleFileName path hash)

-- | Retrieves the meta information for the handle
getBundleHistory :: StorageConf -> BundleHash -> IO [BundleHistory]
getBundleHistory path hash =
	read <$> B.unpack <$> B.readFile (bundleDir path </> hash <.> "data")

-- | Adds a new entry to the bundle history (stamped with the current time)
changeBundleState :: StorageConf -> BundleHash -> Source -> BundleState -> IO ()
changeBundleState path hash source state = do
	history <- getBundleHistory path hash
	now <- getCurrentTime
	B.writeFile (bundleDir path </> hash <.> "data") $ B.pack $ show $
		(now, source, state) : history
		

listBundles :: StorageConf -> IO [BundleHash]
listBundles path = do
	items <- getDirectoryContents (bundleDir path)
	return $ map dropExtension
	       $ filter ( (".bundle" == ) .takeExtension )
	       $ items



updateRepository :: StorageConf -> (String -> IO ()) -> RepositoryURL -> IO Bool
updateRepository path write repo = do
	let infoFile = repoDir path </> safeName repo <.> "data"
	let repoFile = repoDir path </> safeName repo <.> "inventory"
	ex <- doesFileExist infoFile
	info <- if ex then read . B.unpack <$> B.readFile infoFile
                      else return (RepositoryInfo Nothing Nothing)
	now <- getCurrentTime
	(ps,changed) <- getInventory write (cacheDir path) repo
	when changed $ B.writeFile repoFile (make_context ps)
	B.writeFile infoFile $ B.pack $ show $ RepositoryInfo
		{ lastCheck  = Just now
		, lastUpdate = if changed then Just now else lastUpdate info
		}
	return changed

readRepository :: StorageConf -> RepositoryURL -> IO [PatchInfo]
readRepository path repo = do
	let repoFile = repoDir path </> safeName repo <.> "inventory"
	ex <- doesFileExist repoFile
	if ex then scan_context `fmap` B.readFile repoFile
	      else return []

getRepositoryInfo :: StorageConf -> RepositoryURL -> IO RepositoryInfo
getRepositoryInfo path repo = do
	let infoFile = repoDir path </> safeName repo <.> "data"
	ex <- doesFileExist infoFile
	if ex then read . B.unpack <$> B.readFile infoFile
	      else return (RepositoryInfo Nothing Nothing)

setBundleListList :: StorageConf -> [BundleList] -> IO ()
setBundleListList path bll = do
	writeFile (bundleListDir path </> "index")  (show bll)

getBundleListList :: StorageConf -> IO [BundleList]
getBundleListList path = do
	read <$> B.unpack <$> B.readFile (bundleListDir path </> "index")

writeBundleList :: StorageConf -> BundleList -> [BundleHash] -> IO ()
writeBundleList path bl blist = do
	let filename = bundleListFilename path bl
	    sorted = sort blist
	(oldList,_) <- readBundleList path bl
	when (oldList /= blist) $ do
		now <- getCurrentTime
		writeFile filename (unlines (show now : blist))

readBundleList :: StorageConf -> BundleList -> IO ([BundleHash], UTCTime)
readBundleList path bl = do
	let filename = bundleListFilename path bl
	ex <- doesFileExist filename	
	if not ex then return ([],UTCTime (ModifiedJulianDay 0) 0) else do
		(stamp:bhashes) <- lines <$> B.unpack <$> B.readFile filename
		return (bhashes, read stamp)

writeNameMapping :: StorageConf -> M.Map Author String -> IO ()
writeNameMapping path map = do
	B.writeFile (nameMappingFilename path) $ B.pack $ show map

readNameMapping :: StorageConf -> IO (M.Map Author String)
readNameMapping path = do
	let filename = nameMappingFilename path
	ex <- doesFileExist filename	
	if not ex then return M.empty else 
		read <$> B.unpack <$> B.readFile filename

bundleListFilename path (RepositoryBundleList repo) =
	bundleListDir path </> "repo_" ++ safeName repo
bundleListFilename path (AuthorBundleList author) =
	bundleListDir path </> "user_" ++ safeName author
bundleListFilename path UnmatchedBundleList =
	bundleListDir path </> "unmatched"

nameMappingFilename path = path </> "nameMapping"

bundleDir :: StorageConf -> FilePath
bundleDir path = path </> "bundles"

bundleBaseName :: BundleHash -> FilePath
bundleBaseName hash = hash <.> "bundle"

bundleFileName :: StorageConf -> BundleHash -> FilePath
bundleFileName path hash  = bundleDir path </> bundleBaseName hash

repoDir :: StorageConf -> FilePath
repoDir path = path </> "repos"

cacheDir :: StorageConf -> FilePath
cacheDir path = path </> "cache"

bundleListDir :: StorageConf -> FilePath
bundleListDir path = path </> "bundleLists"

safeName n = map s n
  where s c = if isSafeFileChar c then c else '_'

isSafeFileChar c = isAlpha c || isDigit c || c `elem` "-_.@:"
