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
import Data.Time
import System.FilePath

import qualified Data.ByteString.Char8 as B
import Data.ByteString.Char8 (ByteString)

-- Darcs stuff
import Darcs
import Darcs.Watch.GenerateOutput
import Darcs.Watch.Data
import LockRestart

import Data.Digest.OpenSSL.MD5 (md5sum)
import Data.Maybe


main = do
	hSetBuffering stdout NoBuffering
        args <- getArgs
        let (confdir, patchNew) = case args of
                        [confdir] -> (addSlash confdir, False)
                        [confdir, "new"] -> (addSlash confdir, True)
                        _         -> error "Use darcswatch confdir/"
        putStrLn "Reading configuration..."
        config <- read `fmap` readFile (confdir ++ "config")

	lockRestart (cData config) patchNew or True (generateOutput config)
