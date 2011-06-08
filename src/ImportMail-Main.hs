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

import qualified Data.Map as M
import System.Environment
import Control.Monad
import Data.Maybe
import System.FilePath
import System.Posix.IO
import Safe

import Darcs.Watch.Data
import Darcs.Watch.ImportMail
import Darcs.Watch.UpdateRepoData
import Darcs.Watch.GenerateOutput
import LockRestart

main = do
	args <- getArgs
        let (confdir) = case args of
                        [confdir] -> (confdir)
                        _         -> error "Use import-mail confdir/ and pipe mail to it"
        config <- readNote "reading Configuration" `fmap` readFile (confdir </> "config")
	foundMail <- importMail config
	when foundMail $ do
		-- We do not want output here
		nullFd <- openFd "/dev/null" WriteOnly Nothing defaultFileFlags 
		dupTo nullFd stdOutput
		
		lockRestart (cData config) False or False $ \new -> do
			updateRepoData config
			generateOutput config new
