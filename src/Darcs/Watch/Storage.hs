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
	B.writeFile (bundleDir path </> hash <.> "bundle")
	            (make_bundle bundle)
	return hash


-- | Retrieves a new patch bundle from the storage
getBundle :: StorageConf -> BundleHash -> IO PatchBundle
getBundle path hash = 
	either error id . scan_bundle <$> B.readFile (bundleDir path </> hash <.> "bundle")

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

bundleDir :: StorageConf -> FilePath
bundleDir path = path </> "bundles"

getBundleFileName :: StorageConf -> BundleHash -> FilePath
getBundleFileName path hash  = bundleDir path </> hash <.> "bundle"

repoDir :: StorageConf -> FilePath
repoDir path = path </> "repos"

cacheDir :: StorageConf -> FilePath
cacheDir path = path </> "cache"

safeName n = map s n
  where s c = if isSafeFileChar c then c else '_'

isSafeFileChar c = isAlpha c || isDigit c || c `elem` "-_.@:"
