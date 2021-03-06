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

import Data.List
import Data.Maybe
import System.Environment
import System.FilePath
import Network.CGI
import Control.Applicative
import Safe

import Darcs.Watch.Storage
import Darcs.Watch.Data
import HTML

main = do
	confdir <- getEnv "HTTP_DARCSWATCH_DIR"

        config <- readNote "reading Configuration" `fmap` readFile (confdir </> "config") :: IO DarcsWatchConfig
	
	runCGI $ handleErrors $ authenticated config $ \openID -> do

	--setHeader "Content-type" "text/html; charset=UTF-8"

	bundleChanges <- getBundleChanges <$> getInputs
	if null bundleChanges
	 then output $ cgiMessagePage True "No changed entered"
	 else do errors <- catMaybes <$> mapM (applyBundleChange config openID) bundleChanges
	         if null errors
		  then output $ cgiMessagePage False $ "Sucessfully updated " ++
			show (length bundleChanges) ++ " patch bundle state" ++
			(if (length bundleChanges) == 1 then "" else "s")++ ". " ++
			"The DarcsWatch web pages are generated " ++
			"periodically, so it might take a while until your "++
			"changes become visible."
		  else output $ cgiMessagePage True $ "There were errors "++
			"applying your updates: "++ show errors

getBundleChanges = mapMaybe $ \(n,v) -> 
	case ("state-" `stripPrefix` n, v) of
		(Nothing,_) -> Nothing
		(Just hb,"UNCHANGED") -> Nothing
		(Just hb,"OBSOLETE") -> Just (hb, Obsoleted)
		(Just hb,"REJECTED") -> Just (hb, Rejected)
		_ -> Nothing 

applyBundleChange config openID (bhash,newState) = do
	history <- liftIO $ getBundleHistory (cData config) bhash
	if newState <= maxState history then
		return $ Just $
			"Can not set patch bundle state to " ++ show newState
			++", "++ "already in state " ++ show (maxState history) ++ "!"
	  else do
	  	liftIO $ changeBundleState (cData config) bhash (ViaWeb openID) newState
		return Nothing

authenticated config action = do
	action "unauthenticated"
