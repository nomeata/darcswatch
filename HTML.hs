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
	, mainPage
	, userPage
	, repoPage
	, userFile
	, repoFile
	) where

import StringCrypto (md5)

import Text.XHtml hiding ((!))
import qualified Text.XHtml ((!))
import Text.Printf
import qualified Data.Map as M
import Data.Map ((!))
import qualified MultiMap as MM
import MultiMap ((!!!!))
import Data.List
import Data.Ord
import System.Time

import Darcs


-- not in ghc6.6
infixl 0 `on`
on :: (b -> b -> c) -> (a -> b) -> a -> a -> c
(*) `on` f = \x y -> f x * f y

data ResultData = ResultData
	{ p2r ::  M.Map PatchInfo [String]
	, r2p ::  M.Map String    [PatchInfo]
	, u2p ::  M.Map String    [PatchInfo]
	, p2c ::  M.Map PatchInfo [PatchInfo]
	, p2d ::  M.Map PatchInfo String
	, p2pr :: M.Map PatchInfo [String]
	, r2mp :: M.Map String    [PatchInfo]
	, unapplicable :: [PatchInfo]
	, date :: CalendarTime
	}

users d = M.keys (u2p d)
repos d = M.keys (r2p d)

mainPage d = showHtml $
   header << thetitle << "DarcsWatch overview" +++
   body << (
	h1 << "DarcsWatch overview" +++
	h2 << "Statistics" +++
	p << stringToHtml (
		printf "Tracking %d repositories and %d patches submitted by %d users"
		(M.size (r2p d)) (M.size (p2d d)) (M.size (u2p d))
		) +++
	h2 << "Listing by user" +++ 
	unordList (map (\u -> hotlink (userFile u) << u +++ userStats u d) (users d)) +++
	h2 << "Listing by repository" +++ 
	unordList (map (\r -> hotlink (repoFile r) << r +++ repoStats r d) (repos d)) +++
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
   	thetitle << ("DarcsWatch overview for " +++ u) +++
	myHeader d
	) +++
   body << (
	h1 << ("DarcsWatch overview for " +++ u) +++
	p << hotlink "." << "Return to main page" +++
	patchList d (sps !!!! NotApplied) "Unapplied patches" True +++
	patchList d (sps !!!! Unmatched) "Unmatched patches" True +++
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
	footer d
	)
  where (ps, sps) = repoData r d
 
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
	 else   (	" " +++ small << (" by " +++ hotlink (userFile (piAuthor p)) << piAuthor p)
	 	)
	) +++
	pre << unlines (piLog p) +++
	(if userCentric
	 then	(unordList $ flip map (p2pr d !!!! p) $ \r ->
			hotlink (repoFile r) << r +++ ": "+++ viewState d p r
		)
	 else	noHtml
	) +++
	thediv !!! [identifier diffId, thestyle "display:none"] << (
		actions +++
		pre << p2d d !!!! p
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
data PatchState = Unmatched | Applied | NotApplied | Obsolete | Obsoleting deriving (Eq, Ord)

state d p r | p `elem` ps                          = Applied
            | ip `elem` ps                         = NotApplied
            | ip `elem` subs && not (piInverted p) = Obsolete
            | ip `elem` subs &&      piInverted p  = Obsoleting
            | otherwise                            = NotApplied
  where context = p2c d ! p
  	ps = r2p d ! r
        ip = inversePatch p
	subs = M.keys (p2d d)

instance Show PatchState where
	show Unmatched = "Unmatched"
	show Applied = "Applied"
	show NotApplied = "Not yet applied"
	show Obsolete = "Marked obsolete"
	show Obsoleting = "Marking patch as obsolete"

stateColor Applied = "green"
stateColor NotApplied = "red"
stateColor Obsolete = "gray"
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
	count Obsolete "obsolete"
  where (ps, sps) = userData u d
  	count s t = case sps !!!! s of
	              [] -> noHtml
	              _  -> inColor (stateColor s) << (show (length ps) +++ " " +++ t) +++ " "
	  where ps = (sps !!!! s)

repoStats r d = " " +++
	show (length (r2p d !!!! r)) +++ 
	" patches in inventory "+++
	count Applied "tracked" +++
	count NotApplied "applicable" +++
	count Obsolete "obsolete"
  where (ps, sps) = repoData r d
  	count s t = case sps !!!! s of
	              [] -> noHtml
	              _  -> inColor (stateColor s) << (show (length ps) +++ " " +++ t) +++ " "
	  where ps = (sps !!!! s)

repoData r d = (ps, sorted) 
  where ps = patchSort $ r2mp d !!!! r
	sorted = MM.fromList $ map (\p -> (state d p r, p)) ps

userData u d = (ps, sorted)
  where ps = patchSort $ u2p d !!!! u
	sorted = MM.fromList $ map (\p -> (state' p, p)) ps
	state' p | null repos = Unmatched
	         | otherwise  = maximum (map (state d p) repos)
	  where repos = p2pr d !!!! p

userFile u = "user_" ++ md5 u ++ ".html"
repoFile r = "repo_" ++ md5 r ++ ".html"


patchSort = reverse . sortBy (compare `on` piDate)
