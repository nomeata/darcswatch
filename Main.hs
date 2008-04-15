
import Control.Monad
import System
import System.Directory
import System.Time

import Network.HTTP
import Network.URI

import qualified Data.Map as M

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
	let confdir = addSlash $ case args of
			[confdir] -> confdir
			_         -> error "Use darcswatch confdir/"
	putStrLn "Reading configuration..."
	config <- read `fmap` readFile (confdir ++ "config")
	putStrLn "Reading repositories..."
	let readInv (p2r,r2p) rep = do 
		putStrLn $ "Reading " ++ rep ++ " ..." 
		invFile <- getInventory (rep ++ "/_darcs/inventory")
		ps <- parseInventory invFile
		let p2r' = foldr (\p -> M.insertWith (++) p [rep]) p2r ps
		    r2p' = M.insertWith (++) rep ps r2p
		return (p2r', r2p')
	(p2r,r2p) <- foldM readInv (M.empty, M.empty) (cRepositories config)
	
	putStrLn "Reading emails..."
	mailFiles <- getDirectoryFiles (cMails config)

	let readMail (u2p, p2c, p2d) mailFile = do
		putStrLn $ "Reading mail " ++ mailFile ++ " ..."
		(new,context) <- parseMail mailFile
		let u2p' = foldr (\(p,_) -> M.insertWith (++) (piAuthor p) [p]) u2p new
		    p2c' = foldr (\(p,_) -> M.insert p context) p2c new 
		    p2d' = foldr (\(p,d) -> M.insert p d) p2d new
		return (u2p', p2c', p2d')
	(u2p, p2c, p2d) <- foldM readMail (M.empty, M.empty, M.empty) mailFiles

	let patches = M.keys p2d -- Submitted patches
	let repos   = M.keys r2p -- Repos with patches
	let users   = M.keys u2p -- Known users

	let addables = do -- List modad
		patch <- patches
		repo  <- repos
		context <- M.lookup patch p2c
		present <- M.lookup repo r2p
		guard $ all (`elem` present) context
		return (patch, repo)
	-- Patch to possible repos
	-- Repo to possible patch
	let p2pr = foldr (\(p,r) -> M.insertWith (++) p [r]) M.empty addables
	let r2mp = foldr (\(p,r) -> M.insertWith (++) r [p]) M.empty addables

	-- Unapplicable patches
	let unapplicable = filter (\p -> not (M.member p p2pr)) patches

	let resultData = ResultData p2r r2p u2p p2c p2d p2pr r2mp unapplicable
	putStrLn "Writing output..."
	now <- getClockTime >>= toCalendarTime
 	writeFile (cOutput config ++ "/index.html") (mainPage resultData now)
 
 	forM_ users $ \u ->
 		writeFile (cOutput config ++ "/" ++ userFile u) (userPage resultData u)
 
 	forM_ repos $ \r ->
 		writeFile (cOutput config ++ "/" ++ repoFile r) (repoPage resultData r)
	return ()

	

getInventory :: String -> IO String
getInventory file | head file == '/' = readFile file	
                  | otherwise        = do
		  	let Just uri = parseURI file
		  	result <- simpleHTTP (Request
				{ rqURI = uri
				, rqMethod = GET
				, rqHeaders = []
				, rqBody = ""
				})
			return $ case result of
				Left err ->       ""
				Right response -> rspBody response

getDirectoryFiles dir' = getDirectoryContents dir >>=
			return . (map (dir++)) >>=
			return . filter ((/= '.') . head) >>=
			filterM doesFileExist  >>=
			filterM ((readable `fmap`) . getPermissions)
  where	dir = addSlash dir'
		

addSlash filename | last filename == '/' = filename
                  | otherwise            = filename ++ "/"
