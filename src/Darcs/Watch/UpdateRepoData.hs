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

module Darcs.Watch.UpdateRepoData where

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

import qualified Data.ByteString.Char8 as B
import Data.ByteString.Char8 (ByteString)
import Data.Digest.OpenSSL.MD5 (md5sum)
import Data.Maybe
import System.FilePath
import Control.Monad.Writer

-- Darcs stuff
import Darcs
import Darcs.Watch.Storage
import Darcs.Watch.Data
import Darcs.Watch.Roundup
import HTML (normalizeAuthor)

newtype BundleListMap = BundleListMap { unBundleListMap :: M.Map BundleList (S.Set BundleHash) }

instance Monoid BundleListMap where
	mempty = BundleListMap M.empty
	mappend (BundleListMap m1) (BundleListMap m2) =
		BundleListMap (M.unionWith (S.union) m1 m2)

addBundleListEntry bl bh = tell (BundleListMap (M.singleton bl (S.singleton bh)))

forBundleListMap :: Monad m => BundleListMap -> (BundleList -> [BundleHash] -> m ()) -> m ()
forBundleListMap (BundleListMap m) act = mapM_ (\(bl,bhs) -> act bl (S.toList bhs)) $ M.toList m

updateRepoData config = do
	let readRepo rep = do
		inv <- readRepository (cData config)  rep
		return (rep, S.fromList inv)
	repos <- mapM readRepo (cRepositories config)

	bundleHashes <- listBundles (cData config)
	putStrLn "Detecing bundle state changes..."
	bundleHashLists <- execWriterT $ forM_ bundleHashes $ \bundleHash -> do
		bundle  <- liftIO $ getBundle (cData config) bundleHash
		history <- liftIO $ getBundleHistory (cData config) bundleHash

		let context = snd bundle
		let patches = map fst (fst bundle)

		forM patches $ \pi -> do
			let email = B.unpack (normalizeAuthor (piAuthor pi))
			addBundleListEntry (AuthorBundleList email) bundleHash
		
		forM repos $ \(repo, inv) -> do
			let statusQuoAnte = stateOfRepo history repo
			    statusQuo = 
				if all (`S.member` inv) patches then Applied
				else if applicable inv context then Applicable
				else New

			when (statusQuo /= statusQuoAnte) $ liftIO $ do
				putStrLn $ "Marking bundle " ++ bundleHash ++
			  		  " as " ++ show statusQuo ++
					  " with regard to " ++ repo
				changeBundleState (cData config)
						  bundleHash
						  (ViaRepository repo)
						  statusQuo
				
				case haveRoundupURL history of
					Nothing -> return ()
					Just url -> 
					  	tellRoundup config url repo bundle statusQuo

			when (statusQuo /= New) $
				addBundleListEntry (RepositoryBundleList repo) bundleHash

	
	putStrLn $ "Writing bundle lists..."
	forBundleListMap bundleHashLists $ \bl bh -> do
		writeBundleList (cData config) bl bh	

-- Clonsider patches as applicable to a repository when either
--  * all its context is in the repository
--  * at least 10 patches of the context are in the repository
applicable inv context = 
	case partition (`S.member` inv) context of
		(_,[])                 -> True
		(m,_) | length m >= 10 -> True
		_                      -> False
