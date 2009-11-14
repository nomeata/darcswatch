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

-- Darcs stuff
import Darcs
import Darcs.Watch.Storage
import Darcs.Watch.Data
-- Web ouput
import HTML
--import LockRestart



main = do
	hSetBuffering stdout NoBuffering
        args <- getArgs
        let confdir = case args of
                        [confdir] -> confdir
                        _         -> error "Use darcswatch confdir/"
        putStrLn "Reading configuration..."
        config <- read `fmap` readFile (confdir </> "config")

	--lockRestart (cOutput config) patchNew or True (do_work config)
	do_work config

do_work config = do

	let readRepo rep = do
		inv <- readRepository (cData config)  rep
		return (rep, S.fromList inv)
	repos <- mapM readRepo (cRepositories config)

	bundleHashes <- listBundles (cData config)
	forM_ bundleHashes $ \bundleHash -> do
		bundle <- getBundle (cData config) bundleHash
		history <- getBundleHistory (cData config) bundleHash

		let context = snd bundle
		let patches = map fst (fst bundle)
		
		forM repos $ \(repo, inv) -> do
			let statusQuoAnte =
				fromMaybe New $
				listToMaybe $
				map (\(_,_,s) -> s) $
				filter (aboutRepo repo) $
				history
			
			let statusQuo = 
				if all (`S.member` inv) patches then Applied
				else if applicable inv context then Applicable
				else New

			when (statusQuo /= statusQuoAnte) $ do
				putStrLn $ "Marking bundle " ++ bundleHash ++
			  		  " as " ++ show statusQuo ++
					  " with regard to " ++ repo
				changeBundleState (cData config)
						  bundleHash
						  (ViaRepository repo)
						  statusQuo

-- Clonsider patches as applicable to a repository when either
--  * all its context is in the repository
--  * at least 10 patches of the context are in the repository
applicable inv context = 
	case partition (`S.member` inv) context of
		(_,[])                 -> True
		(m,_) | length m >= 10 -> True
		_                      -> False


aboutRepo repo (_,ViaRepository r,_) = repo == r
aboutRepo _    _                     = False
