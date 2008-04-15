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

userPage d u = showHtml $
   header << thetitle << ("DarcsWatch overview for " +++ u) +++
   body << (
	h1 << ("DarcsWatch overview for " +++ u) +++
	patchList d unappPatches "Unapplied patches" True +++
	patchList d unmatched "Unmatched patches" True +++
	patchList d appPatches "Applied patches" True +++
	footer d
	)
  where (ps, unmatched, appPatches, unappPatches) = userData u d

repoPage d r = showHtml $
   header << thetitle << ("DarcsWatch overview for " +++ r) +++
   body << (
	h1 << ("DarcsWatch overview for " +++ r) +++
	patchList d unappPatches "Unapplied patches" False +++
	patchList d appPatches "Applied patches" False +++
	footer d
	)
  where (ps, appPatches, unappPatches) = repoData r d
 
patchList d [] title userCentric = h5 << ("No "++title)
patchList d ps title userCentric = 
	h2 << title +++ (unordList $ map (patchView d userCentric) ps)

patchView d userCentric p =
	piDate p +++ ": " +++ strong << piName p +++
	(if userCentric
	 then	noHtml
	 else   (	" " +++ small << (" by " +++ hotlink (userFile (piAuthor p)) << piAuthor p)
	 	)
	) +++
	pre << unlines (piLog p) +++
	(if userCentric
	 then	(unordList $ flip map (M.findWithDefault [] p (p2pr d)) $ \r ->
			hotlink (repoFile r) << r +++ ": "+++ state d p r
		)
	 else	noHtml
	) +++
	paragraph << (	strong << "Actions: " +++
			hotlink (patchBasename p ++ ".dpatch") << "Download .dpatch"
			)
		

applied d p r = p `elem` (r2p d ! r)

state d p r | applied d p r = thespan !!! [thestyle "color:green"] << "Applied"
	    -- recognize obsolete patches
            | otherwise     = thespan !!! [thestyle "color:red"]  << "Not yet applied"

(!!!) = (Text.XHtml.!)	    


userStats u d = stringToHtml $ printf
	" %d tracked patches, %d unapplied, %d unmatched" -- , %d unapplied"
	(length ps)
	(length unappPatches)
	(length unmatched)
  where (ps, unmatched, appPatches, unappPatches) = userData u d

repoStats r d = stringToHtml $ printf
	" %d patches in inventory, %d tracked, %d applicable" 
	(length (M.findWithDefault [] r (r2p d)))
	(length (ps))
	(length (unappPatches))
  where (ps, appPatches, unappPatches) = repoData r d

repoData r d = (ps, appPatches, unappPatches) 
  where ps = patchSort $ M.findWithDefault [] r (r2mp d)
  	(appPatches, unappPatches) = partition (\p -> applied d p r) ps

userData u d = (ps, unmatched, appPatches, unappPatches)
  where ps = patchSort $ M.findWithDefault [] u (u2p d)
  	(unmatched, ps') = partition (\p -> null ((M.findWithDefault [] p (p2pr d)))) ps
  	(appPatches, unappPatches) = partition (
		\p -> all (\r -> applied d p r) (M.findWithDefault [] p (p2pr d))
		) ps'

userFile u = "user_" ++ md5 u ++ ".html"
repoFile r = "repo_" ++ md5 r ++ ".html"


patchSort = reverse . sortBy (compare `on` piDate)
