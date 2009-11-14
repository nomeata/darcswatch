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

import qualified Data.ByteString.Char8 as B
import Data.ByteString.Char8 (ByteString)

-- Darcs stuff
import Darcs
import Darcs.Watch.Storage
import Darcs.Watch.Data
-- Web ouput
import HTML
import LockRestart

import Data.Digest.OpenSSL.MD5 (md5sum)
import Data.Maybe

data DarcsWatchConfig = DarcsWatchConfig {
        cRepositories :: [RepositoryURL],
	cData :: String,
        cOutput :: String,
        cMails :: String
        } deriving (Show, Read)


main = do
	hSetBuffering stdout NoBuffering
        args <- getArgs
        let (confdir, patchNew) = case args of
                        [confdir] -> (addSlash confdir, False)
                        [confdir, "new"] -> (addSlash confdir, True)
                        _         -> error "Use darcswatch confdir/"
        putStrLn "Reading configuration..."
        config <- read `fmap` readFile (confdir ++ "config")

	lockRestart (cOutput config) patchNew or True (do_work config)

do_work config patchNew = do
	writeC <- getConcurrentOutputter

        putStrLn "Reading repositories..."
	let loadInv rep = do
                writeC $ "Reading " ++ rep ++ ":\n"
                (ps,thisNew) <- getInventory writeC (cOutput config ++ "/cache/") rep
		writeC (if thisNew then "Repostory is new.\n" else "Repository is cached.\n")
		return (rep, ps, thisNew)
            readInv (p2r,r2p,new) (rep, ps,thisNew) = do
                let p2r' = foldr (\p -> MM.append p rep) p2r ps
                    r2p' = MM.extend rep ps r2p :: M.Map RepositoryURL (S.Set PatchInfo)
                return (p2r', r2p', new || thisNew)
        (p2r,r2p, new) <- foldM readInv (MM.empty, MM.empty, patchNew) =<<
                          forkSequence (map loadInv (cRepositories config))

        if not new then putStrLn "Nothing new, exiting" else do

        putStrLn "Reading emails..."
	bundleHashes <- listBundles (cData config)

        let sortInBundle (u2p, u2rn, p2pe, p2s) bundleHash = do
                putStrLn $ "Reading mail " ++ bundleHash ++ " ..."
		bundle <- getBundle (cData config) bundleHash
		history <- getBundleHistory (cData config) bundleHash

                let (new,context) = bundle
                let u2p' = foldr (\(p,_) -> MM.append (normalizeAuthor (piAuthor p)) p) u2p new
                let u2rn' = foldr (\(p,_) ->
                        M.insertWith (maxBy B.length) (normalizeAuthor (piAuthor p)) (piAuthor p)
                        ) u2rn new
                let p2pe' = foldr (\(p,d) ->
                        let pe = PatchExtras d context bundleHash
                        -- The patch with the smaller context is the more useful
                        in  M.insertWith (minBy (length.peContext)) p pe
                        ) p2pe new
		let state = bundleState2patchState $ case history of
			[] -> New
			(_,_,s):_ -> s
		let p2s' = foldr (\(p,_) -> M.insert p state) p2s new
                return (u2p', u2rn', p2pe', p2s)
        (u2p, u2rn, p2pe, p2s) <- foldM sortInBundle (MM.empty, M.empty, M.empty, M.empty) bundleHashes
        let patches = M.keys p2pe -- Submitted patches
        let repos   = M.keys r2p -- Repos with patches
        let users   = M.keys u2p -- Known users

	-- Clonsider patches as belonging to a repository when either
	--  * it is already applied
	--  * all its context is in the repository
	--  * at least 10 patches of the context are in the repository
        let addables = do -- List monad
                patch <- patches
                repo  <- repos
                present <- maybeToList $ M.lookup repo r2p
		if patch `S.member` present
                  then return (patch,repo)
		  else do
			pe <- maybeToList $ M.lookup patch p2pe
			case partition (`S.member` present) (peContext pe) of
				(_,[]) -> return (patch,repo)	
				(m,_) | length m >= 10 -> return (patch,repo)	
				_ -> []

        -- Patch to possible repos
        -- Repo to possible patch
        let p2pr = foldr (\(p,r) -> MM.append p r) MM.empty addables
        let r2mp = foldr (\(p,r) -> MM.append r p) MM.empty addables

        -- Unmatched patches
        let unmatched = S.fromList $
			-- filter (\p -> M.findWithDefault Unmatched p p2s == Unmatched) $
                        filter (\p -> not (M.member p p2pr)) patches

        now <- getClockTime >>= toCalendarTime
        let resultData = ResultData p2r r2p u2p p2pe p2pr r2mp p2s unmatched now u2rn
	
	{-
	putStrLn "Evalutating data"
	putStrLn (show (length (show (resultData))))
	-}

        putStrLn "Writing output..."
        writeFile (cOutput config ++ "/index.html") (mainPage resultData)

        forM_ users $ \u ->
                writeFile (cOutput config ++ "/" ++ userFile u) (userPage resultData u)

        forM_ repos $ \r ->
                writeFile (cOutput config ++ "/" ++ repoFile r) (repoPage resultData r)

        forM_ patches $ \p ->
                B.writeFile (cOutput config ++ "/" ++ patchDiffFile p) (peDiff (p2pe M.! p))

	writeFile (cOutput config ++ "/" ++ "unmatched.html") (unmatchedPage resultData)

        putStrLn "Linking patches"
        let patchLink (p,pe) = do
                let link = cOutput config ++ "/" ++ patchBasename p ++ ".dpatch"
                ex <- fileExist link
                unless ex $ do
                        -- There might be a broken symlink here:
                        catch (removeFile link) (const (return ()))
                        createSymbolicLink (peMailFile pe) link
        mapM_ patchLink $ M.toList p2pe

        return ()

getDirectoryFiles dir' = getDirectoryContents dir >>=
                        return . (map (dir++)) >>=
                        return . filter ((/= '.') . head) >>=
                        filterM doesFileExist  >>=
                        filterM ((readable `fmap`) . getPermissions)
  where dir = addSlash dir'

maxBy f v1 v2 = if f v1 >= f v2 then v1 else v2
minBy f v1 v2 = if f v1 <= f v2 then v1 else v2


addSlash filename | last filename == '/' = filename
                  | otherwise            = filename ++ "/"

-- not in ghc6.6
infixl 0 `on`
on :: (b -> b -> c) -> (a -> b) -> a -> a -> c
(*) `on` f = \x y -> f x * f y

{- forkSequence = sequence -}
-- Enable for parallel downloads
forkSequence acts = mapM (\act -> newEmptyMVar >>= \mvar -> forkIO (act >>= putMVar mvar) >> return mvar) acts >>= mapM takeMVar

bundleState2patchState Darcs.Watch.Data.New = Unmatched
bundleState2patchState Darcs.Watch.Data.Obsoleted = Obsolete
bundleState2patchState Darcs.Watch.Data.Rejected = HTML.Rejected
