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

import qualified Data.ByteString.Char8 as B
import Data.ByteString.Char8 (ByteString)

-- | Given a directory to be used for caching the result, and
--   an URL to download, it will return the content of the URL.
--   The boolean return value is true when the file was updated,
--   and False if it is the same as in the cache.
get :: FilePath -> String -> IO (Maybe (ByteString, Bool))
get dir' uri = flip catch (\e -> putStrLn ("Error downloading uri: " ++ show e) >> return Nothing) $ do
	putStr $ "Getting URL " ++ uri ++ " ... "
	e_cache <- doesFileExist cacheFile
	e_tag   <- doesFileExist tagFile
	let update = do
		newFile <- get' uri
		case newFile of
		  Nothing -> do
			putStrLn "not found."
			return Nothing
		  Just (newFile, Nothing) -> do
			putStrLn "downloaded."
			return $ Just (newFile, True)
		  Just (newFile, Just newTag) -> do
			putStrLn "downloaded."
			B.writeFile cacheFile newFile
			writeFile tagFile newTag
			return $ Just (newFile, True)

	if False -- Debugging
	  then do
		oldFile <- B.readFile cacheFile
		return $ Just (oldFile, False)
           else if e_cache && e_tag
		   then do
			oldTag <- slurpFile tagFile
			mbNewTag <- head' uri
			maybe'' mbNewTag (return Nothing) $ \newTag -> do
				if  oldTag == newTag
				   then do
					putStrLn "cached."
					oldFile <- B.readFile cacheFile
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
			[ uaHeader
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
			[ uaHeader
			]
		, rqBody = B.empty
		}
	result <- simpleHTTP req
	return $ case result of
		Left err -> Nothing
		Right response -> case rspCode response of
			(2,_,_) -> Just $ (rspBody response, lookupHeader HdrLastModified (rspHeaders response))
			_       -> Nothing

uaHeader = Header HdrUserAgent "DarcsWatch http://darcswatch.nomeata.de/"

slurpFile file = do
	c <- readFile file
	if (c == c) then return c else error "This should not happen"
		

addSlash filename | last filename == '/' = filename
                  | otherwise            = filename ++ "/"
