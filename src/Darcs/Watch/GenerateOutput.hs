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

module Darcs.Watch.GenerateOutput where

import Control.Monad
import Control.Concurrent
import Control.Applicative
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
import Data.Time
import System.FilePath

import qualified Data.ByteString.Char8 as B
import Data.ByteString.Char8 (ByteString)

-- Darcs stuff
import Darcs
import Darcs.Watch.Storage
import Darcs.Watch.Data
-- Web ouput
import HTML

import Data.Digest.OpenSSL.MD5 (md5sum)
import Data.Maybe

generateOutput config patchNew = do
	nowStamp <- getCurrentTime
	let outputStampFile = cData config </> "output.stamp"
	ex <- doesFileExist outputStampFile
	lastStamp <- if ex then read . B.unpack <$> B.readFile outputStampFile
	                   else return $ UTCTime (ModifiedJulianDay 0) 0

        putStrLn "Reading repositories..."
	let loadInv rep = do
                putStr $ "Reading " ++ rep ++ " ...\n"
		ps <- readRepository (cData config) rep
		repoInfo <- getRepositoryInfo (cData config) rep
		let thisNew = maybe True (>= lastStamp) (lastUpdate repoInfo)
		putStr (if thisNew then "Repostory is new.\n" else "Repository is cached.\n")
		return (rep, ps, repoInfo, thisNew)
            readInv (p2r,r2p,r2ri,new) (rep, ps, repoInfo, thisNew) = do
                let p2r' = foldr (\p -> MM.append p rep) p2r ps
                    r2p' = MM.extend rep ps r2p :: M.Map RepositoryURL (S.Set PatchInfo)
		    r2ri' = M.insert rep repoInfo r2ri
                return (p2r', r2p', r2ri', new || thisNew)
        (p2r,r2p, r2ri, new) <- foldM readInv (MM.empty, MM.empty, M.empty, patchNew) =<<
                          sequence (map loadInv (cRepositories config))

        putStrLn "Reading emails..."
	bundleHashes <- listBundles (cData config)

        let sortInBundle (u2p, u2rn, p2pe, new) bundleHash = do
                putStrLn $ "Reading mail " ++ bundleHash ++ " ..."
		bundle <- getBundle (cData config) bundleHash
		let bundleFileName = getBundleFileName (cData config) bundleHash
		history <- getBundleHistory (cData config) bundleHash
		
		let thisNew = any (\(d,_,_) -> d >= lastStamp) history

                let (patches,context) = bundle
                let u2p' = foldr (\(p,_) -> MM.append (normalizeAuthor (piAuthor p)) p) u2p patches
                let u2rn' = foldr (\(p,_) ->
                        M.insertWith (maxBy B.length) (normalizeAuthor (piAuthor p)) (piAuthor p)
                        ) u2rn patches
                let p2pe' = foldr (\(p,d) ->
                        let pe = PatchExtras d context bundleFileName history
                        -- The patch with the smaller context is the more useful
                        in  M.insertWith (minBy (length.peContext)) p pe
                        ) p2pe patches
                return (u2p', u2rn', p2pe', new || thisNew)
        (u2p, u2rn, p2pe, new') <- foldM sortInBundle (MM.empty, M.empty, M.empty, new) bundleHashes
        let patches = M.keys p2pe -- Submitted patches
        let repos   = M.keys r2p -- Repos with patches
        let users   = M.keys u2p -- Known users
        
	if not new' then putStrLn "Nothing new, exiting" else do


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
        let resultData = ResultData p2r r2p u2p p2pe p2pr r2mp r2ri unmatched now u2rn
	
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
                        createSymbolicLink (peBundleFile pe) link
        mapM_ patchLink $ M.toList p2pe

	B.writeFile outputStampFile (B.pack (show nowStamp))

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
