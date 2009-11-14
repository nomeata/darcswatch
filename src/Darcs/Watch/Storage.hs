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
import Data.Time
import qualified Data.ByteString.Char8 as B
import Data.ByteString.Char8 (ByteString)
import System.FilePath
import System.Directory
import Data.List
import Control.Applicative
import System.Directory
import Control.Monad

type BundleHash = String

-- | the path to the storage directory
type StorageConf = String

-- | A history entry is a state change
type BundleHistory = (UTCTime, Source, BundleState)

data BundleState
	= New
	| Rejected
	| Obsoleted
	| Applied -- ^ Repository URL
	deriving (Read, Show)
	
data Source
	= ViaEMail String String String String -- ^ From, To, Subject, Message-Id 
	| ViaBugtracker String -- ^ URL
--	| ViaWeb String -- ^ OpenID Username
	| ViaRepository String -- ^ Repository-URL
	deriving (Read, Show)

-- | Adds a new patch bundle to the stogare
addBundle :: StorageConf -> PatchBundle -> IO BundleHash
addBundle path bundle = do
	let hash = hash_bundle bundle
	let dataFile = bundleDir path </> hash <.> "data"
	ex <- doesFileExist dataFile
	unless ex $ writeFile dataFile "[]"
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
	read <$> readFile (bundleDir path </> hash <.> "data")

-- | Adds a new entry to the bundle history (stamped with the current time)
changeBundleState :: StorageConf -> BundleHash -> Source -> BundleState -> IO ()
changeBundleState path hash source state = do
	history <- getBundleHistory path hash
	now <- getCurrentTime
	writeFile (bundleDir path </> hash <.> "data") $ show $
		(now, source, state) : history
		

listPatches :: StorageConf -> IO [BundleHash]
listPatches path = do
	items <- getDirectoryContents (bundleDir path)
	return $ map dropExtension
	       $ filter ( (".bundle" == ) .takeExtension )
	       $ items

bundleDir :: StorageConf -> FilePath
bundleDir path = path </> "bundles"
