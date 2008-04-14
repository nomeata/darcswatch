module HTML 
	( ResultData(ResultData)
	, mainPage
	, userPage
	, repoPage
	, userFile
	, repoFile
	) where

import MD5String (md5)

import Text.XHtml hiding ((!))
import qualified Text.XHtml ((!))
import Text.Printf
import qualified Data.Map as M
import Data.Map ((!))
import Data.List
import Data.Ord
--import Data.Function

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
	unordList (map (\r -> hotlink (repoFile r) << r +++ repoStats r d) (repos d))

   )

userPage d u = showHtml $
   header << thetitle << ("DarcsWatch overview for " +++ u) +++
   body << (
	h1 << ("DarcsWatch overview for " +++ u) +++
	(unordList $ flip map ps $ \p ->
		patchView d p +++
		(unordList $ flip map (M.findWithDefault [] p (p2pr d)) $ \r ->
			r +++ ": "+++ state d p r
		)
			
	))
  where ps = patchSort $ M.findWithDefault [] u (u2p d)

repoPage d r = showHtml $
   header << thetitle << ("DarcsWatch overview for " +++ r) +++
   body << (
	h1 << ("DarcsWatch overview for " +++ r) +++
	h2 << ("Unapplied patches") +++
	(unordList $ flip map unappPatches $ \p ->
		patchView d p
	) +++
	h2 << ("Applied patches") +++
	(unordList $ flip map appPatches   $ \p ->
		patchView d p
	)
	)
  where ps = patchSort $ M.findWithDefault [] r (r2mp d)
  	(appPatches, unappPatches) = partition (\p -> applied d p r) ps

patchView d p =
	piDate p +++ ": " +++ strong << piName p +++
	pre << unlines (piLog p)

applied d p r = p `elem` (r2p d ! r)

state d p r | applied d p r = thespan !!! [thestyle "color:green"] << "Applied"
	    -- recognize obsolete patches
            | otherwise     = thespan !!! [thestyle "color:red"]  << "Not yet applied"

(!!!) = (Text.XHtml.!)	    


userStats u d = stringToHtml $ printf
	" %d total patches" -- , %d unapplied"
	(length (M.findWithDefault [] u (u2p d))) -- ? 

repoStats r d = stringToHtml $ printf
	" %d patches in inventory, %d tracked, %d applicable" 
	(length (M.findWithDefault [] r (r2p d)))
	(length (ps))
	(length (unappPatches))
  where ps = patchSort $ M.findWithDefault [] r (r2mp d)
  	(appPatches, unappPatches) = partition (\p -> applied d p r) ps

userFile u = "user_" ++ md5 u ++ ".html"
repoFile r = "repo_" ++ md5 r ++ ".html"

patchSort = sortBy (compare `on` piDate)
