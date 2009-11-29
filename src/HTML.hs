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
	( BundleInfo(..)
	, mainPage
	, userPage
	, repoPage
	, cgiMessagePage
	, userFile
	, repoFile
	, patchDiffFile
	, normalizeAuthor
	) where

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
import Data.Maybe
import Data.Time
import System.Time
import System.Locale

import qualified Data.ByteString.Char8 as B
import Data.ByteString.Char8 (ByteString)

import Darcs
import Darcs.Watch.Data


-- not in ghc6.6
infixl 0 `on`
on :: (b -> b -> c) -> (a -> b) -> a -> a -> c
(*) `on` f = \x y -> f x * f y

data BundleInfo = BundleInfo
	{ biBundleHash :: BundleHash
	, biBundle :: PatchBundle
	, biFileName :: FilePath
	, biHistory :: [BundleHistory]
	}
	deriving (Eq)

instance Ord BundleInfo where
	compare = compare `on` (map (piDate.fst) . fst . biBundle)

mainPage date
         numPatches
	 repoData
	 authorData
	= showHtml $
   header << (
   	thetitle << "DarcsWatch overview" +++
	myHeader 
	) +++
   body << (
	h1 << "DarcsWatch overview" +++
	p << ( "Welcome to DarcsWatch. If you want to know more, please read the " +++
	       hotlink "http://darcs.nomeata.de/darcswatch/documentation.html" << "Documentation" +++
	       "."
	       ) +++
	h2 << "Statistics" +++
	p << stringToHtml (
		printf "Tracking %d repositories and %d patches submitted by %d users"
		(length repoData) (numPatches::Integer) (length authorData)
		) +++
	h2 << "Listing by user" +++ 
	unordList (map userLine authorData) +++
	h2 << "Listing by repository" +++ 
	unordList (map repoLine repoData) +++
	footer (Just date)
	)
  where userLine (u, tracked, unapplied, obsolete, rejected) =
 		hotlink (userFile (B.pack u)) << u +++ -- TODO realname
		" " ++
		show tracked +++ 
		" tracked patches, "+++
		count Applicable "unapplied" unapplied +++
		count Obsoleted "obsolete" obsolete +++
		count Rejected "rejected" rejected
        repoLine (r, inventory, tracked, applicable, obsolete, rejected) =
 		hotlink (repoFile r) << r +++
		" " ++
		show tracked +++ 
		" patches in inventory, "+++
		count Applied "tracked" tracked +++
		count Applicable "applicable" applicable +++
		count Obsoleted "obsolete" obsolete +++
		count Rejected "rejected" rejected
	count s t 0 = noHtml
	count s t n = inColor (stateColor s) << (show n +++ " " +++ t) +++ " "

footer mDate = 
	p !!! [thestyle "font-size:80%"] << (
		"darcswatch © Joachim Breitner <" +++
		hotlink "mailto:mail@joachim-breitner.de" << "mail@joachim-breitner.de" +++
		">. Source code at " +++
		hotlink "http://darcs.nomeata.de/darcswatch/" << "http://darcs.nomeata.de/darcswatch/"+++
		"." +++ maybe noHtml (\d -> " Last update " +++ calendarTimeToString d +++ ".") mDate
		)

myHeader   = script !!! [thetype "text/javascript", src "/javascript/jquery/jquery.js"] << noHtml
             +++
             script !!! [thetype "text/javascript"] << "\
		\$(document).ready(function () { \
                \   $('.diffshower').click(function () { \
                \       var diffid = this.id.replace(/^diffshower_/,''); \
                \       $('#diff_' + diffid).toggle(); \
		\       $('#diff_' + diffid + ' pre:visible').each(function () {\
		\           diffelem = this;\
		\           if (!($(diffelem).text())) {\
		\               $(diffelem).text('Diff is being loaded...');\
		\               jQuery.get('diff_' + diffid + '.txt',\
		\                       callback=function(text) {\
		\                              $(diffelem).text(text);\
        	\                       }\
		\                       ,type='text');\
		\           }});\
                \   })})\
		\"
             +++
	     style !!! [thetype "text/css" ] << "\
	     	\span[title] { border-bottom: dotted 1px black; } \
		\"

userPage date u bundleInfos = showHtml $
   header << (
   	thetitle << ("DarcsWatch overview for " +++ u) +++
	myHeader
	) +++
   body << form !!! [ method "GET", action "cgi"] << (
	h1 << ("DarcsWatch overview for " +++ u) +++ -- TODO: Mapping to real name
	p << hotlink "." << "Return to main page" +++
	bundleList "Unapplied patch bundles" (bundleInfoFilter Applicable bundleInfos) +++ 
	bundleList "Unmatched patch bundles" (bundleInfoFilter New bundleInfos) +++ 
	bundleList "Applied patch bundles" (bundleInfoFilter Applied bundleInfos) +++ 
	bundleList "Obsoleted patch bundles" (bundleInfoFilter Obsoleted bundleInfos) +++ 
	bundleList "Rejected patch bundles" (bundleInfoFilter Rejected bundleInfos) +++ 
	footer (Just date)
	)


repoPage date r repoInfo bundleInfos = showHtml $
   header << (
   	thetitle << ("DarcsWatch overview for " +++ r) +++
	myHeader
	) +++
   body << form !!! [ method "GET", action "cgi"] << (
	h1 << ("DarcsWatch overview for " +++ r) +++
	p << hotlink "." << "Return to main page" +++
	bundleList "Unapplied patch bundles" (bundleInfoFilter Applicable bundleInfos) +++ 
	bundleList "Applied patch bundles" (bundleInfoFilter Applied bundleInfos) +++ 
	bundleList "Obsoleted patch bundles" (bundleInfoFilter Obsoleted bundleInfos) +++ 
	bundleList "Rejected patch bundles" (bundleInfoFilter Rejected bundleInfos) +++ 
	thediv << (
		"Last change in repository at " +++
		maybe (toHtml "No idea when") toHtml (lastUpdate repoInfo) +++
		", last check of repository at " +++
		maybe (toHtml "No idea when") toHtml (lastCheck repoInfo) +++ "."
	) +++
	footer (Just date)
	)

bundleList title [] = noHtml
bundleList title list = 
	h2 << title +++
	unordList (map bundleView list)

bundleInfoFilter state = sort . filter (\bi -> state == maxState (biHistory bi))

{-
unmatchedPage d = showHtml $
   header << (
   	thetitle << ("DarcsWatch overview, unmatched patches") +++
	myHeader
	) +++
   body << form !!! [ method "GET", action "cgi"] << (
	h1 << ("DarcsWatch overview, unmatched Patches") +++
	p << hotlink "." << "Return to main page" +++
	patchList d (S.toList (unmatched d)) "Unmatched patches" False +++
	footer (Just (date d))
	)
-}

cgiMessagePage isError msg = showHtml $
   header << (
   	thetitle << "DarcsWatch" +++
	myHeader
	) +++
   body << (
	h1 << "DarcsWatch" +++
	p << (
		(if isError then inColor "red" << "Error: " else noHtml)
		+++
		msg
	      ) +++
	footer Nothing
	)

patchHistoryView history = unordList $
	map (\(date,source,state) ->
		date +++ ": " +++ showState state +++ " " +++ showSource source)
	    (reverse history)

showSource (ViaRepository repo) = " in repo " +++ hotlink (repoFile repo) << repo
showSource (ViaBugtracker url) = " in bug tracker ticket " +++ hotlink url << url
showSource (ViaWeb openid) = " via web interface by user " +++ hotlink openid << openid
showSource (ViaEMail from to subject mmid) = " via " +++
					     thespan !!! [ title $
					     	"From: " ++ from ++ "\n" ++
						"Subject: " ++ subject
					     ] << "e-mail" +++ " to " +++
					     hotlink ("mailto:" ++ to) << to  +++
					     maybe noHtml (\mid ->
						" " +++ hotlink ("http://mid.gmane.org/" ++ mid) 
					    	      (image !!!
						    	[ src "http://gmane.org/favicon.ico"
							, title "Search this mail on gmane.org"
							, border 0
							])
					     ) mmid

showSource ManualImport = toHtml "via a manual import"

bundleView (BundleInfo bundleHash (ps,ctx) bundleFileName history) = 
	p << (strong << "Contents:" +++
	     " (" +++
	     (if length ps == 1
	      then "1 patch"
	      else show (length ps) ++ " patches" ) +++
	     ")"
	) +++
	unordList (map patchView (map fst ps)) +++
	p << (strong << "History:") +++
	patchHistoryView history +++
	actions
  where
	actions = paragraph << (
			strong << "Actions: " +++
			hotlink bundleFileName << "Download .dpatch" +++
			" "+++ 
			if maxState history <= Applicable
			then	" "+++
				select !!! [ name ("state-" ++ bundleHash), size "1"] << (
					option !!! [ value "UNCHANGED" ] << "Mark patch as..." +++
					option !!! [ value "OBSOLETE" ] << "Obsolete" +++
					option !!! [ value "REJECTED" ] << "Rejected"
					) +++
				submit "submit" "Submit"
			else noHtml
			)

patchView p =
	piDate p +++ ": " +++ strong << (
		(if piInverted p then stringToHtml "UNDO: " else noHtml) +++
		piName p
		) +++
	" " +++
	small << (
		" by " +++
		hotlink (userFile (piAuthor p)) << piAuthor p +++
		" " +++
		diffShower
	) +++
	(if null (stripIgnorethis (piLog p)) then noHtml else pre << B.unlines (piLog p)) +++
	thediv !!! [identifier diffId, thestyle "display:none"] << pre noHtml
  where pid = patchBasename p
	diffShowerId = "diffshower_"++pid
	diffId = "diff_"++pid
	diffShower = anchor
		!!! [identifier diffShowerId, theclass "diffshower", href "javascript:"]
		<< "Show/Hide diff"
	stripIgnorethis [] = []
	stripIgnorethis (x:xs) = if B.pack "Ignore-this:" `B.isPrefixOf` x 
	                         then xs else (x:xs)

maxState history = maximum $ New : map (\(_,_,s) -> s) history

{-
state d p r | p `S.member` ps                          = Applied
            | ip `S.member` ps                         = maxState history
	    | amend_obsoleted                          = Obsoleted
            | ip `S.member` subs && not (piInverted p) = Obsoleted
            | otherwise                                = maxState history
  where context = peContext (p2pe d ! p)
	history = peStateHistory (p2pe d ! p)
  	ps = r2p d ! r
        ip = inversePatch p
	subs = M.keysSet (p2pe d)
	amend_obsoleted = any (`laterThan` p) $ S.toList (u2p d ! (normalizeAuthor (piAuthor p)))

p1 `laterThan` p2 =    piAuthor p1 == piAuthor p2
                    && piName p1   == piName p2
		    && piLog p1    == piLog p2
		    && piInverted p1 == piInverted p2
		    && piDate p1   > piDate p2
-}

showState New = "Seen bundle"
showState Applicable = "Not yet fully applied bundle"
showState Rejected = "Bundle marked rejected"
showState Obsoleted = "Bundle marked obsolete"
showState Applied = "Bundle fully applied"

stateColor New = "black"
stateColor Applicable = "red"
stateColor Obsoleted = "gray"
stateColor Rejected = "brown"
stateColor Applied = "green"

inColor c = thespan !!! [thestyle ("color:"++c)]

(!!!) :: ADDATTRS a => a -> [HtmlAttr] -> a
(!!!) = (Text.XHtml.!)	    

{-
userStats u d = " " +++
	show (length ps) +++ 
	" tracked patches, "+++
	count Applicable "unapplied" +++
	count Obsoleted "obsolete" +++
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
	count Applicable "applicable" +++
	count Obsoleted "obsolete" +++
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
	state' p | S.null repos = New
	         | otherwise    = S.findMax (S.map (state d p) repos)
	  where repos = p2pr d !!!! p
-}

userFile u = "user_" ++ B.unpack (normalizeAuthor u) ++ ".html"
repoFile r = "repo_" ++ safeName r ++ ".html"
patchDiffFile p = "diff_" ++ patchBasename p ++ ".txt"

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

instance HTML UTCTime where
	toHtml = toHtml . formatTime defaultTimeLocale "%c" 
