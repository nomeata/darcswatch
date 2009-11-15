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

module Darcs.Watch.PullRepos where

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


pullRepos config = do
	writeC <- getConcurrentOutputter

        putStrLn "Updating repositories..."
	let updateRepo rep = do
                writeC $ "Updating " ++ rep ++ ":\n"
		thisNew <- updateRepository (cData config) writeC rep
		writeC (if thisNew then "Repostory is new.\n" else "Repository is cached.\n")
		return thisNew
	new <- or <$> forkSequence (map updateRepo (cRepositories config))

        if not new then putStrLn "Nothing new, exiting" else return ()

{- forkSequence = sequence -}
-- Enable for parallel downloads
forkSequence acts = mapM (\act -> newEmptyMVar >>= \mvar -> forkIO (act >>= putMVar mvar) >> return mvar) acts >>= mapM takeMVar
