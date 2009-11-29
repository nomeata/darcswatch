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

module Darcs.Watch.GenerateOutput where

import Control.Monad
import Control.Concurrent
import Control.Applicative
import System.Environment (getArgs)
import System.Directory
import System.Posix.Files
import System.Time
import System.IO
import System.Terminal.Concurrent

import qualified Data.Map as M
import qualified Data.Set as S
import qualified MultiMap as MM
import MultiMap ((!!!!))
import Data.Char
import Data.List
import Data.Time
import Data.Digest.OpenSSL.MD5 (md5sum)
import Data.Maybe
import Data.Monoid
import System.FilePath

import qualified Data.ByteString.Char8 as B
import Data.ByteString.Char8 (ByteString)

-- Darcs stuff
import Darcs
import Darcs.Watch.Storage
import Darcs.Watch.Data
-- Web ouput
import HTML

generateOutput config patchNew = do
	nowStamp <- getCurrentTime
	let outputStampFile = cData config </> "output.stamp"
	ex <- doesFileExist outputStampFile
	lastStamp <- if ex then read . B.unpack <$> B.readFile outputStampFile
	                   else return $ UTCTime (ModifiedJulianDay 0) 0

	date <- getClockTime >>= toCalendarTime

	putStrLn "Reading name mapping list..."
	nameMapping <- readNameMapping (cData config)

	putStrLn "Reading bundle list lists.."
	bundleLists <- getBundleListList (cData config)
	-- Split bundle lists by type
	let (repos, authors) = mconcat $ map (\bl -> case bl of
		RepositoryBundleList repo -> ([repo],[])
		AuthorBundleList author -> ([],[author])
		_ -> mempty) bundleLists
	
	let getBundleInfos bundleHash = do
		bundle <- getBundle (cData config) bundleHash
		let bundleFileName = getBundleFileName (cData config) bundleHash
		history <- getBundleHistory (cData config) bundleHash
		
		return (BundleInfo bundleHash bundle bundleFileName history)

        putStrLn "Writing output (repo pages)..."
        forM_ repos $ \r -> do
		(bundleHashes,_) <- readBundleList (cData config) (RepositoryBundleList r)
		bundleInfos <- mapM getBundleInfos bundleHashes
		repoInfo <- getRepositoryInfo (cData config) r
                writeFile (cOutput config ++ "/" ++ repoFile r) $
			repoPage date nameMapping r repoInfo bundleInfos

        putStrLn "Writing output (user pages)..."
        forM_ authors $ \u -> do
		(bundleHashes,_) <- readBundleList (cData config) (AuthorBundleList u)
		bundleInfos <- mapM getBundleInfos bundleHashes
                writeFile (cOutput config ++ "/" ++ userFile u) $
			userPage date nameMapping u bundleInfos
        
	putStrLn "Writing output (main page)..."
	bundleHashes <- listBundles (cData config)

	patches <- mconcat <$> forM bundleHashes (\bundleHash -> do
		(ps,_) <- getBundle (cData config) bundleHash
		return (M.fromList ps)
		)
	let patchCount = M.size patches
	let bundleCount = length bundleHashes
	repoData <- forM repos $ \r -> do
		(bundleHashes,_) <- readBundleList (cData config) (RepositoryBundleList r)
		bundleInfos <- mapM getBundleInfos bundleHashes
		inv <- readRepository (cData config) r
		return (r,
			length inv,
			length $ bundleInfoFilter Applied bundleInfos,
			length $ bundleInfoFilter Applicable bundleInfos,
			length $ bundleInfoFilter Obsoleted bundleInfos,
			length $ bundleInfoFilter Rejected bundleInfos
			)
	userData <- forM authors $ \u -> do
		(bundleHashes,_) <- readBundleList (cData config) (AuthorBundleList u)
		bundleInfos <- mapM getBundleInfos bundleHashes
		return (u,
			length bundleInfos,
			length $ bundleInfoFilter Applicable bundleInfos,
			length $ bundleInfoFilter Obsoleted bundleInfos,
			length $ bundleInfoFilter Rejected bundleInfos
			)
        writeFile (cOutput config ++ "/index.html") $
		mainPage date nameMapping patchCount bundleCount repoData userData

        putStrLn "Writing output (diffs)..."
        forM_ (M.toList patches) $ \(p,d) -> do
		let filename = cOutput config ++ "/" ++ patchDiffFile p 
		ex <- doesFileExist filename
		unless ex $ do
			putStrLn $ "Writing new patch file " ++ filename
			B.writeFile filename d

{-
        putStrLn "Linking patches"
        let patchLink (p,pe) = do
                let link = cOutput config ++ "/" ++ patchBasename p ++ ".dpatch"
                ex <- fileExist link
                unless ex $ do
                        -- There might be a broken symlink here:
                        catch (removeFile link) (const (return ()))
                        createSymbolicLink (peBundleFile pe) link
        mapM_ patchLink $ M.toList p2pe
-}


	B.writeFile outputStampFile (B.pack (show nowStamp))

getDirectoryFiles dir' = getDirectoryContents dir >>=
                        return . (map (dir++)) >>=
                        return . filter ((/= '.') . head) >>=
                        filterM doesFileExist  >>=
                        filterM ((readable `fmap`) . getPermissions)
  where dir = addSlash dir'

maxBy f v1 v2 = if f v1 >= f v2 then v1 else v2
minBy f v1 v2 = if f v1 <= f v2 then v1 else v2


addSlash filename | last filename == '/' = filename
                  | otherwise            = filename ++ "/"

-- not in ghc6.6
infixl 0 `on`
on :: (b -> b -> c) -> (a -> b) -> a -> a -> c
(*) `on` f = \x y -> f x * f y
