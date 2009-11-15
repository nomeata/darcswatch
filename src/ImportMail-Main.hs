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
import Codec.MIME.String as MIME
import Text.Regex

import Darcs.Watch.Data
import Darcs.Watch.Storage
import Darcs.Watch.ImportMail

main = do
	args <- getArgs
        let (confdir) = case args of
                        [confdir] -> (confdir)
                        _         -> error "Use convert confdir/ and pipe mail to it"
        config <- read `fmap` readFile (confdir </> "config")
	importMail config
