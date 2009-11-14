{-
Copyright (C) 2009 Joachim Breitner

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

import Darcs
import qualified Data.Map as M
import Data.Digest.OpenSSL.MD5 (md5sum)
import Data.List
import System.Environment
import System.Directory
import Control.Monad
import qualified Data.ByteString.Char8 as B
import Data.Maybe
import System.FilePath

import Darcs.Watch.Data
import Darcs.Watch.Storage

main = do
	args <- getArgs
        let (confdir, file) = case args of
                        [confdir, file] -> (confdir, file)
                        _         -> error "Use convert confdir/ patchfile"
        putStrLn "Reading configuration..."
        config <- read `fmap` readFile (confdir </> "config")

	putStrLn "Reading patch bundle..."
	mail <- B.readFile file
	let bundle = parseMail mail
	bhash <- addBundle (cData config) bundle
	changeBundleState (cData config) bhash ManualImport New
	putStrLn $ "Sucessfully added " ++ bhash

