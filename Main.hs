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
import System
import System.Directory
import System.Posix.Files
import System.Time

import qualified Data.Map as M
import qualified MultiMap as MM

-- Darcs stuff
import Darcs
-- Web ouput
import HTML

data DarcsWatchConfig = DarcsWatchConfig {
	cRepositories :: [String],
	cOutput :: String,
	cMails :: String
	} deriving (Show, Read)


main = do
	args <- getArgs
	let (confdir, patchNew) = case args of
			[confdir] -> (addSlash confdir, False)
			[confdir, "new"] -> (addSlash confdir, True)
			_         -> error "Use darcswatch confdir/"
	putStrLn "Reading configuration..."
	config <- read `fmap` readFile (confdir ++ "config")
	putStrLn "Reading repositories..."
	let readInv (p2r,r2p,new) rep = do 
		putStrLn $ "Reading " ++ rep ++ " ..." 
		(ps,thisNew) <- getInventory (cOutput config ++ "/cache/") rep
		let p2r' = foldr (\p -> MM.append p rep) p2r ps
		    r2p' = MM.extend rep ps r2p
		return (p2r', r2p', new || thisNew)
	(p2r,r2p, new) <- foldM readInv (MM.empty, MM.empty, patchNew) (cRepositories config)

	if not new then putStrLn "Nothing new, exiting" else do
	
	putStrLn "Reading emails..."
	mailFiles <- getDirectoryFiles (cMails config)

	let readMail (u2p, p2pe) mailFile = do
		putStrLn $ "Reading mail " ++ mailFile ++ " ..."
		(new,context) <- parseMail mailFile
		let u2p' = foldr (\(p,_) -> MM.append (piAuthor p) p) u2p new
		let p2pe' =  foldr (\(p,d) ->
			let pe = PatchExtras d context mailFile 
			-- The patch with the smaller context is the more useful
			    choosePe pe1 pe2 = 
				if length (peContext pe1) > length (peContext pe2)
				then pe2
				else pe1
			in  M.insertWith choosePe p pe) p2pe new 
		return (u2p', p2pe')
	(u2p, p2pe) <- foldM readMail (MM.empty, M.empty) mailFiles

	let patches = M.keys p2pe -- Submitted patches
	let repos   = M.keys r2p -- Repos with patches
	let users   = M.keys u2p -- Known users

	let addables = do -- List modad
		patch <- patches
		repo  <- repos
		pe <- M.lookup patch p2pe
		present <- M.lookup repo r2p
		guard $ all (`elem` present) (peContext pe)
		return (patch, repo)
	-- Patch to possible repos
	-- Repo to possible patch
	let p2pr = foldr (\(p,r) -> MM.append p r) MM.empty addables
	let r2mp = foldr (\(p,r) -> MM.append r p) MM.empty addables

	-- Unapplicable patches
	let unapplicable = filter (\p -> not (M.member p p2pr)) patches

	now <- getClockTime >>= toCalendarTime
	let resultData = ResultData p2r r2p u2p p2pe p2pr r2mp unapplicable now
	putStrLn "Writing output..."
 	writeFile (cOutput config ++ "/index.html") (mainPage resultData)
 
 	forM_ users $ \u ->
 		writeFile (cOutput config ++ "/" ++ userFile u) (userPage resultData u)
 
 	forM_ repos $ \r ->
 		writeFile (cOutput config ++ "/" ++ repoFile r) (repoPage resultData r)

	putStrLn "Linking patches"
	let patchLink (p,pe) = do
		let link = cOutput config ++ "/" ++ patchBasename p ++ ".dpatch"
		ex <- fileExist link
		unless ex $ createSymbolicLink (peMailFile pe) link
	mapM_ patchLink $ M.toList p2pe

	return ()

getDirectoryFiles dir' = getDirectoryContents dir >>=
			return . (map (dir++)) >>=
			return . filter ((/= '.') . head) >>=
			filterM doesFileExist  >>=
			filterM ((readable `fmap`) . getPermissions)
  where	dir = addSlash dir'
		

addSlash filename | last filename == '/' = filename
                  | otherwise            = filename ++ "/"

-- not in ghc6.6
infixl 0 `on`
on :: (b -> b -> c) -> (a -> b) -> a -> a -> c
(*) `on` f = \x y -> f x * f y
