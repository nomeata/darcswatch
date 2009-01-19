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
import System.Environment (getArgs)
import System.Directory
import System.Posix.Files
import System.Time

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
-- Web ouput
import HTML
import LockRestart

import Data.Digest.OpenSSL.MD5 (md5sum)

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

	lockRestart (cOutput config) patchNew or True (do_work config)

do_work config patchNew = do
        putStrLn "Reading repositories..."
        let readInv (p2r,r2p,new) rep = do
                putStrLn $ "Reading " ++ rep ++ " ..."
                (ps,thisNew) <- getInventory (cOutput config ++ "/cache/") rep
                let p2r' = foldr (\p -> MM.append p rep) p2r ps
                    r2p' = MM.extend rep ps r2p :: M.Map String (S.Set PatchInfo)
                return (p2r', r2p', new || thisNew)
        (p2r,r2p, new) <- foldM readInv (MM.empty, MM.empty, patchNew) (cRepositories config)

        if not new then putStrLn "Nothing new, exiting" else do

        putStrLn "Reading emails..."
        mailFiles' <- getDirectoryFiles (cMails config)
        let mailFiles = filter ((addSlash (cMails config) ++ "patch") `isPrefixOf`) mailFiles'

        let readMail (u2p, u2rn, p2pe, md2p) mailFile = do
                putStrLn $ "Reading mail " ++ mailFile ++ " ..."
                mail <- B.readFile mailFile
                let checksum = md5sum mail
                let (new,context) = parseMail mail
                let u2p' = foldr (\(p,_) -> MM.append (normalizeAuthor (piAuthor p)) p) u2p new
                let u2rn' = foldr (\(p,_) ->
                        M.insertWith (maxBy B.length) (normalizeAuthor (piAuthor p)) (piAuthor p)
                        ) u2rn new
                let p2pe' =  foldr (\(p,d) ->
                        let pe = PatchExtras d context mailFile
                        -- The patch with the smaller context is the more useful
                        in  M.insertWith (minBy (length.peContext)) p pe
                        ) p2pe new
                let md2p' = MM.extend checksum (map fst new) md2p
                return (u2p', u2rn', p2pe', md2p')
        (u2p, u2rn, p2pe, md2p) <- foldM readMail (MM.empty, M.empty, M.empty, MM.empty) mailFiles

        putStrLn "Reading bundle states..."
        states <- readFile (addSlash (cMails config) ++ "states")
        let readStateLine string =
                let (checksum: stateString : _ : rest) = words string
                    sender = unwords rest
                    state = case stateString of
                                "add" -> Unmatched
                                "obsolete" -> Obsolete
                                "rejected" -> Rejected
                                unknown    -> error $ "Unknown state " ++ show unknown
                in  flip (foldr (\p -> M.insert p state)) (md2p !!!! checksum)
            p2s = foldl (flip readStateLine) M.empty (lines states)

	putStrLn "Reading mid to patch mapping..."
        midmappings <- readFile (addSlash (cMails config) ++ "mid-mapping")
        let readMidLine string =
                let [mid,checksum] = words string
                in  flip (foldr (\p -> M.insert p mid)) (md2p !!!! checksum)
            p2mid = foldl (flip readMidLine) M.empty (lines midmappings)

        let patches = M.keys p2pe -- Submitted patches
        let repos   = M.keys r2p -- Repos with patches
        let users   = M.keys u2p -- Known users

        let addables = do -- List modad
                patch <- patches
                repo  <- repos
                pe <- M.lookup patch p2pe
                present <- M.lookup repo r2p
                guard $ all (`S.member` present) (peContext pe)
                return (patch, repo)
        -- Patch to possible repos
        -- Repo to possible patch
        let p2pr = foldr (\(p,r) -> MM.append p r) MM.empty addables
        let r2mp = foldr (\(p,r) -> MM.append r p) MM.empty addables

        -- Unmatched patches
        let unmatched = S.fromList $ filter (\p -> not (M.member p p2pr)) patches

        now <- getClockTime >>= toCalendarTime
        let resultData = ResultData p2r r2p u2p p2pe p2pr r2mp p2s p2mid unmatched now u2rn
	
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
