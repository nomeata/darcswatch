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

module HTML 
	( ResultData(ResultData)
	, PatchExtras(..)
	, mainPage
	, userPage
	, repoPage
	, unmatchedPage
	, userFile
	, repoFile
	, normalizeAuthor
	, PatchState(..)
	) where

import StringCrypto (md5)

import Text.XHtml hiding ((!))
import qualified Text.XHtml ((!))
import Text.Printf
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Map ((!))
import qualified MultiMap as MM
import MultiMap ((!!!!))
import Data.List
import Data.Ord
import Data.Char
import System.Time

import qualified Data.ByteString.Char8 as B
import Data.ByteString.Char8 (ByteString)

import Darcs


-- not in ghc6.6
infixl 0 `on`
on :: (b -> b -> c) -> (a -> b) -> a -> a -> c
(*) `on` f = \x y -> f x * f y

data ResultData = ResultData
	{ p2r ::  M.Map PatchInfo  (S.Set String)
	, r2p ::  M.Map String     (S.Set PatchInfo)
	, u2p ::  M.Map ByteString (S.Set PatchInfo)
	, p2pe::  M.Map PatchInfo   PatchExtras
	, p2pr :: M.Map PatchInfo  (S.Set String)
	, r2mp :: M.Map String     (S.Set PatchInfo)
	, p2s  :: M.Map PatchInfo  (PatchState)
	, unmatched :: (S.Set PatchInfo)
	, date :: CalendarTime
	, u2rn :: M.Map ByteString  String
	}

data PatchExtras = PatchExtras
	{ peDiff :: ByteString
	, peContext :: [PatchInfo]
	, peMailFile :: String
	}


users d = M.keys (u2p d)
repos d = M.keys (r2p d)

mainPage d = showHtml $
   header << thetitle << "DarcsWatch overview" +++
   body << (
	h1 << "DarcsWatch overview" +++
	p << ( "Welcome to DarcsWatch. If you want to know more, please read the " +++
	       hotlink "http://darcs.nomeata.de/darcswatch/documentation.html" << "Documentation" +++
	       "."
	       ) +++
	h2 << "Statistics" +++
	p << stringToHtml (
		printf "Tracking %d repositories and %d patches submitted by %d users"
		(M.size (r2p d)) (M.size (p2pe d)) (M.size (u2p d))
		) +++
	h2 << "Listing by user" +++ 
	unordList (map (\u -> hotlink (userFile u) << u2rn d ! u +++ userStats u d) (users d)) +++
	h2 << "Listing by repository" +++ 
	unordList ( map (\r -> hotlink (repoFile r) << r +++ repoStats r d) (repos d)
			++ [hotlink "unmatched.html" << "Unmatched matches"
			    +++ " "+++  show (S.size (unmatched d)) +++ " patches" ]
		  ) +++
	footer d
	)

footer d = 
	p !!! [thestyle "font-size:80%"] << (
		"darcswatch © Joachim Breitner <" +++
		hotlink "mailto:mail@joachim-breitner.de" << "mail@joachim-breitner.de" +++
		">. Source code at " +++
		hotlink "http://darcs.nomeata.de/darcswatch/" << "http://darcs.nomeata.de/darcswatch/"+++
		". Last update " +++
		calendarTimeToString (date d) +++
		"."
		)
myHeader d = script !!! [thetype "text/javascript"] << (
		" function toggle_diff(id) {  "++
		"    elem = document.getElementById(id); "++
		"    if (elem) {if (elem.style.display == 'none') {elem.style.display = 'block';} else {elem.style.display = 'none'} }"++ 
		" }"
		)


userPage d u = showHtml $
   header << (
   	thetitle << ("DarcsWatch overview for " +++ u2rn d ! u) +++
	myHeader d
	) +++
   body << (
	h1 << ("DarcsWatch overview for " +++ u2rn d ! u) +++
	p << hotlink "." << "Return to main page" +++
	patchList d (sps !!!! NotApplied) "Unapplied patches" True +++
	patchList d (sps !!!! Unmatched) "Unmatched patches" True +++
	patchList d (sps !!!! Rejected) "Rejected patches" True +++
	patchList d (sps !!!! Obsolete) "Obsolete patches" True +++
	patchList d (sps !!!! Applied) "Applied patches" True +++
	footer d
	)
  where (ps, sps) = userData u d

repoPage d r = showHtml $
   header << (
   	thetitle << ("DarcsWatch overview for " +++ r) +++
	myHeader d
	) +++
   body << (
	h1 << ("DarcsWatch overview for " +++ r) +++
	p << hotlink "." << "Return to main page" +++
	patchList d (sps !!!! NotApplied) "Unapplied patches" False +++
	patchList d (sps !!!! Applied) "Applied patches" False +++
	patchList d (sps !!!! Obsolete) "Obsolete patches" False +++
	patchList d (sps !!!! Rejected) "Rejected patches" True +++
	footer d
	)
  where (ps, sps) = repoData r d
 
unmatchedPage d = showHtml $
   header << (
   	thetitle << ("DarcsWatch overview, unmatched patches") +++
	myHeader d
	) +++
   body << (
	h1 << ("DarcsWatch overview, unmatched Patches") +++
	p << hotlink "." << "Return to main page" +++
	patchList d (S.toList (unmatched d)) "Unmatched patches" False +++
	footer d
	)

patchList d [] title userCentric = h5 << ("No "++title)
patchList d ps title userCentric = 
	h2 << title +++ (unordList $ map (patchView d userCentric) ps)

patchView d userCentric p =
	piDate p +++ ": " +++ strong << (
		(if piInverted p then stringToHtml "UNDO: " else noHtml) +++
		piName p
		) +++
	(if userCentric
	 then	noHtml
	 else   (	" " +++ small << (
	 	      " by " +++ hotlink (userFile (piAuthor p)) << piAuthor p
		      )
	 	)
	) +++
	pre << B.unlines (piLog p) +++
	(if userCentric
	 then	(unordList $ flip map (S.toList (p2pr d !!!! p)) $ \r ->
			hotlink (repoFile r) << r +++ ": "+++ viewState d p r
		)
	 else	noHtml
	) +++
	thediv !!! [identifier diffId, thestyle "display:none"] << (
		actions +++
		pre << peDiff (p2pe d ! p)
		) +++
	actions
  where pid = patchBasename p
	diffId = "diff_"++pid
	actions = paragraph << (
			strong << "Actions: " +++
			hotlink (pid ++ ".dpatch") << "Download .dpatch" +++
			" "+++ 
			anchor !!! [href $ "javascript:toggle_diff('"++diffId++"')"]
				<< "Show/Hide diff"
			)
		
-- The order defines what state a patch should be considered if it is
-- in sevaral repositories
data PatchState = Unmatched | Applied | NotApplied | Rejected |  Obsolete | Obsoleting deriving (Eq, Ord)

state d p r | p `S.member` ps                          = Applied
            | ip `S.member` ps                         = explicit_state
	    | amend_obsoleted                          = Obsolete
            | ip `S.member` subs && not (piInverted p) = Obsolete
            | ip `S.member` subs &&      piInverted p  = Obsoleting
            | otherwise                                = explicit_state
  where context = peContext (p2pe d ! p)
  	ps = r2p d ! r
        ip = inversePatch p
	subs = M.keysSet (p2pe d)
	explicit_state = max NotApplied (M.findWithDefault Unmatched p (p2s d))
	amend_obsoleted = any (`laterThan` p) $ S.toList (u2p d ! (normalizeAuthor (piAuthor p)))

p1 `laterThan` p2 =    piAuthor p1 == piAuthor p2
                    && piName p1   == piName p2
		    && piLog p1    == piLog p2
		    && piInverted p1 == piInverted p2
		    && piDate p1   > piDate p2

instance Show PatchState where
	show Unmatched = "Unmatched"
	show Applied = "Applied"
	show NotApplied = "Not yet applied"
	show Rejected = "Marked rejected"
	show Obsolete = "Marked obsolete"
	show Obsoleting = "Marking patch as obsolete"

stateColor Applied = "green"
stateColor NotApplied = "red"
stateColor Obsolete = "gray"
stateColor Rejected = "brown"
stateColor Obsoleting = "gray"
stateColor Unmatched = "black"

inColor c = thespan !!! [thestyle ("color:"++c)]

viewState d p r = inColor (stateColor s) << (show s)
  where	s = state d p r

(!!!) = (Text.XHtml.!)	    


userStats u d = " " +++
	show (length ps) +++ 
	" tracked patches, "+++
	count NotApplied "unapplied" +++
	count Obsolete "obsolete" +++
	count Rejected "rejected"
  where (ps, sps) = userData u d
  	count s t = case sps !!!! s of
	              [] -> noHtml
	              _  -> inColor (stateColor s) << (show (length ps) +++ " " +++ t) +++ " "
	  where ps = (sps !!!! s)

repoStats r d = " " +++
	show (S.size (r2p d !!!! r)) +++ 
	" patches in inventory "+++
	count Applied "tracked" +++
	count NotApplied "applicable" +++
	count Obsolete "obsolete" +++
	count Rejected "rejected"
  where (ps, sps) = repoData r d
  	count s t = case sps !!!! s of
	              [] -> noHtml
	              _  -> inColor (stateColor s) << (show (length ps) +++ " " +++ t) +++ " "
	  where ps = (sps !!!! s)

repoData r d = (ps, sorted) 
  where ps = s2List $ r2mp d !!!! r
	sorted = MM.fromList $ map (\p -> (state d p r, p)) ps

userData u d = (ps, sorted)
  where ps = patchSort $ S.toList $ u2p d !!!! u
	sorted = MM.fromList $ map (\p -> (state' p, p)) ps
	state' p | S.null repos = Unmatched
	         | otherwise    = S.findMax (S.map (state d p) repos)
	  where repos = p2pr d !!!! p

userFile u = "user_" ++ B.unpack (normalizeAuthor u) ++ ".html"
repoFile r = "repo_" ++ safeName r ++ ".html"

normalizeAuthor name | not (B.null r') && valid = email
                     | otherwise                = safeNameB name
  where r' = B.dropWhile (/='<') name
        (email,r'') = B.span (/='>') (B.tail r')
	valid = not (B.null email) && not (B.null r'') && B.all (isSafeFileChar) email

safeNameB n = B.map s n
  where s c = if isSafeFileChar c then c else '_'

safeName n = map s n
  where s c = if isSafeFileChar c then c else '_'

isSafeFileChar c = isAlpha c || isDigit c || c `elem` "-_.@:"


s2List = patchSort . S.toList
patchSort = sortBy (flip (compare `on` piDate))

instance HTML ByteString where
	toHtml = toHtml . B.unpack
