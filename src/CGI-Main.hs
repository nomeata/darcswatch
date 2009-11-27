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

import System.Environment
import System.FilePath
import Network.CGI

import Darcs.Watch.Storage
import Darcs.Watch.Data
import HTML

main = do
	confdir <- getEnv "HTTP_DARCSWATCH_DIR"

        config <- read `fmap` readFile (confdir </> "config") :: IO DarcsWatchConfig

	
	runCGI $ handleErrors $ do

	setHeader "Content-type" "text/html; charset=UTF-8"

	mBundleHash <- getInput "bundlehash"	
	mState <- getInput "state"
	case (mBundleHash, mState) of
	 (Just bhash, Just state) -> case state of
	 	"OBSOLETE" -> cgiSetState config bhash Obsoleted
		"REJECTED" -> cgiSetState config bhash Rejected
		_          -> output $ cgiMessagePage True $ "Unkown state " ++ state
	 _ -> do
	 	output $ cgiMessagePage True "Missing parameters bundlehash or state."

cgiSetState config bhash newState = do
	history <- liftIO $ getBundleHistory (cData config) bhash
	let explicit_state = maximum $ New : map (\(_,_,s) -> s) history
	if newState <= explicit_state then
		output $ cgiMessagePage True $
			"Can not set patch bundle state to " ++ show newState ++", "++
			"already in state " ++ show explicit_state ++ "!"
	  else do
	  	liftIO $ changeBundleState (cData config) bhash (ViaWeb "unauthenticated") newState
		output $ cgiMessagePage False $
			"Sucessfully set patch bundle state to " ++ show newState ++
			". Note that the DarcsWatch pages are updated periodically, " ++
			"so your change may take a while to show up."
