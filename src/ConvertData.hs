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

import Darcs.Watch.Data
import Darcs.Watch.Storage
import Data.Maybe

data DarcsWatchConfig = DarcsWatchConfig {
        cRepositories :: [String],
	cData :: String,
        cOutput :: String,
        cMails :: String
        } deriving (Show, Read)

main = do
	args <- getArgs
        let confdir = case args of
                        [confdir] -> (addSlash confdir)
                        _         -> error "Use convert confdir/"
        putStrLn "Reading configuration..."
        config <- read `fmap` readFile (confdir ++ "config")

	putStrLn "Reading emails..."
        mailFiles' <- getDirectoryFiles (cMails config)
        let mailFiles = filter ((addSlash (cMails config) ++ "patch") `isPrefixOf`) mailFiles'

	putStrLn "Reading bundle states..."
        states <- readFile (addSlash (cMails config) ++ "states")
        let readStateLine string =
                let (checksum: stateString : _ : rest) = words string
                    sender = unwords rest
                    state = case stateString of
                                "add" -> New
                                "obsolete" -> Obsoleted
                                "rejected" -> Rejected
                                unknown    -> error $ "Unknown state " ++ show unknown
                in  M.insert checksum state
    	let p2s = foldl (flip readStateLine) M.empty (lines states) :: M.Map String BundleState

	forM_ mailFiles $ \mailFile -> do
		putStrLn $ "Reading mail " ++ mailFile ++ " ..."
                mail <- B.readFile mailFile
		let bundle = parseMail mail
		let hash = md5sum mail
		putStrLn $ "State is " ++ show (M.lookup hash p2s)
		let state = fromMaybe New (M.lookup hash p2s)
		bhash <- addBundle (cData config) bundle
		changeBundleState (cData config) bhash ManualImport state

addSlash filename | last filename == '/' = filename
                  | otherwise            = filename ++ "/"

getDirectoryFiles dir' = getDirectoryContents dir >>=
                        return . (map (dir++)) >>=
                        return . filter ((/= '.') . head) >>=
                        filterM doesFileExist  >>=
                        filterM ((readable `fmap`) . getPermissions)
  where dir = addSlash dir'
		 
