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

module CachedGet 
	( get
	) where

import StringCrypto
import Network.URI
import Network.HTTP
import Network.HTTP.Headers
import System.Directory
import Control.Monad

get :: FilePath -> String -> IO (Maybe (String, Bool))
get dir' uri = flip catch (\e -> putStrLn ("Error downloading uri: " ++ show e) >> return Nothing) $ do
	e_cache <- doesFileExist cacheFile
	e_tag   <- doesFileExist tagFile
	let update = do
		newFile <- get' uri
		case newFile of
		  Nothing -> do
			return Nothing
		  Just (newFile, Nothing) -> do
			return $ Just (newFile, True)
		  Just (newFile, Just newTag) -> do
			writeFile cacheFile newFile
			writeFile tagFile newTag
			return $ Just (newFile, True)

	if e_cache && e_tag
	   then do
		oldTag <- slurpFile tagFile
		mbNewTag <- head' uri
		maybe'' mbNewTag (return Nothing) $ \newTag -> do
			if oldTag == newTag
			   then do
				oldFile <- readFile cacheFile
				return $ Just (oldFile, False)
			   else do
			   	update
	   else do
	  	update
	
  where hash = md5 uri
 	cacheFile = dir ++ hash ++ ".cache"
        tagFile = dir ++ hash ++ ".tag"
  	dir = addSlash dir'
	maybe'' m d f = maybe d f m


head' uri' = do
	let Just uri = parseURI uri'
	let req = Request
		{ rqURI = uri
		, rqMethod = HEAD
		, rqHeaders =
			[ Header HdrUserAgent "DarcsWatch http://darcswatch.nomeata.de/"
			] 
		, rqBody = ""
		}
	result <- simpleHTTP req
	return $ case result of
		Left err -> Nothing
		Right response -> case rspCode response of 
			(2,_,_) -> lookupHeader HdrLastModified . rspHeaders $ response
			_       -> Nothing

get' uri' = do
	let Just uri = parseURI uri'
	let req = Request
		{ rqURI = uri
		, rqMethod = GET
		, rqHeaders =
			[ Header HdrUserAgent "DarcsWatch http://darcswatch.nomeata.de/"
			] 
		, rqBody = ""
		}
	result <- simpleHTTP req
	return $ case result of
		Left err -> Nothing
		Right response -> case rspCode response of 
			(2,_,_) -> Just $ (rspBody response, lookupHeader HdrLastModified (rspHeaders response))
			_       -> Nothing

slurpFile file = do
	c <- readFile file
	if (c == c) then return c else error "This should not happen"
		

addSlash filename | last filename == '/' = filename
                  | otherwise            = filename ++ "/"
