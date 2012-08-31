{-
Copyright (C) 2009 Joachim Breitner

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

module Darcs.Watch.ImportMail where

import Darcs
import qualified Data.Map as M
import Data.Digest.OpenSSL.MD5 (md5sum)
import Data.List
import System.Environment
import System.Directory
import Control.Monad
import qualified Data.ByteString.Char8 as B
import Data.Maybe
import System.FilePath
import Codec.MIME.String as MIME
import Text.Regex
import Data.Monoid
import Data.Char
import Control.Applicative

import Darcs.Watch.Data
import Darcs.Watch.Storage
import Darcs.Watch.Roundup

importMail :: DarcsWatchConfig -> IO Bool
importMail config  = do
	mail <- getContents
	let message = MIME.parse mail
            mpatch = findDarcsBundle message
	    state = getStateFromMessage message
	    mi = m_message_info message
	    references = getReferredMails mi
	    from = maybe "" (showFrom) (mi_from mi)
	    to   = maybe "" (showTo) (mi_to mi)
	    subject = maybe "" (\(Subject s) -> s) (mi_subject mi)
	    mid = get_header (mi_headers mi) "message-id:"
			     (Just . stripWhitespace )
	    emailSource = ViaEMail from to subject mid
	case mpatch of
		Just bundleData -> do
			let bundle = either error id $ scan_bundle (B.pack bundleData)
			    roundupURL = findRoundupURL message
			bhash <- addBundle (cData config) bundle
			case roundupURL of
			  Nothing -> return ()
			  Just url -> do
				history <- getBundleHistory (cData config) bhash
				forM_ (repoStates history) $ \(repo,state) -> 	
					tellRoundup config url repo bhash bundle state
			  	changeBundleState (cData config) bhash
			  		(ViaBugtracker url) New
			changeBundleState (cData config) bhash emailSource state
			return True

		Nothing -> if (state == New || null references) then return False else do
			-- Maybe this mail refers to another mail?
			bundleHashes <- listBundles (cData config)
			changes <- forM bundleHashes $ \bundleHash -> do
				history <- getBundleHistory (cData config) bundleHash
				let seenInMail = findSubmittingMail history
				if null (intersect references seenInMail)
				  then return False
				  else do changeBundleState (cData config) bundleHash
				  			    emailSource state 
					  return True
			return (or changes)

findSubmittingMail = mapMaybe isSubmittingMail
isSubmittingMail (_,ViaEMail _ _ _ (Just mid), New) = Just mid
isSubmittingMail _ = Nothing

darcsTrackerRegex = mkRegex "\\<(http://bugs.darcs.net/patch[0-9]+)\\>"

getStateFromMessage msg | "OBSOLETE" `isInfixOf` subject = Obsoleted
                        | "REJECTED" `isInfixOf` subject = Rejected
			| otherwise = fromMaybe New (findDarcsWatchTag msg)
  where	mi = m_message_info msg
	subject = maybe "" (\(Subject s) -> s) (mi_subject mi)

getReferredMails :: MessageInfo -> [String]
getReferredMails mi = fromMaybe [] $ mconcat
	[ get_header (mi_headers mi) "in-reply-to:" (Just . (:[]) . stripWhitespace)
	, get_header (mi_headers mi) "references:" (Just . map stripWhitespace . words) ]

stripWhitespace = reverse . dropWhile isSpace . reverse . dropWhile isSpace

findRoundupURL :: Message -> Maybe String
findRoundupURL = findInBody $ fmap concat . matchRegex darcsTrackerRegex

findDarcsWatchTag :: Message -> Maybe BundleState
findDarcsWatchTag = findInBody $ \body -> case () of 
     () | "DarcsWatch: rejected" `isInfixOf` body -> Just Rejected
	| "DarcsWatch: obsolete" `isInfixOf` body -> Just Obsoleted
	| otherwise                               ->  Nothing

findInBody :: (String -> Maybe a) -> Message -> Maybe a
findInBody query (Message _ _ (Body _ _ msg)) = query msg
findInBody query (Message _ _ (Mixed (Multipart _ msgs _ ))) = msum (map (findInBody query) msgs)
findInBody query (Message _ _ (Alternative (Multipart _ msgs _ ))) = msum (map (findInBody query) msgs)
findInBody query (Message _ _ (Parallel (Multipart _ msgs _ ))) = msum (map (findInBody query) msgs)
findInBody query (Message _ _ (Digest (Multipart _ msgs _ ))) = msum (map (findInBody query) msgs)
findInBody query _ = Nothing


findDarcsBundle :: Message -> Maybe String
findDarcsBundle (Message _ _ (Data _ (ContentType "text" "x-darcs-patch" _) _ msg)) = Just msg
findDarcsBundle (Message _ _ (Body (ContentType "text" "x-darcs-patch" _) _ msg)) = Just msg
findDarcsBundle (Message _ _ (Data _ _ filename msg))
	| ".dpatch" `isSuffixOf` filename = Just msg
	| "darcs" `isPrefixOf` filename = Just msg
findDarcsBundle (Message _ _ (Mixed (Multipart _ msgs _ ))) = msum (map findDarcsBundle msgs)
findDarcsBundle (Message _ _ (Alternative (Multipart _ msgs _ ))) = msum (map findDarcsBundle msgs)
findDarcsBundle (Message _ _ (Parallel (Multipart _ msgs _ ))) = msum (map findDarcsBundle msgs)
findDarcsBundle (Message _ _ (Digest (Multipart _ msgs _ ))) = msum (map findDarcsBundle msgs)
findDarcsBundle _ = Nothing

showFrom (From []) = ""
showFrom (From (mb:_)) = showMailbox mb

showTo (To []) = ""
showTo (To (mb:_)) = showAddress mb

showAddress (Address mbox) = showMailbox mbox
showAddress (Group _ [])  = "" 
showAddress (Group _ (mb:_))  = showMailbox mb

showMailbox (Mailbox (Just name) mail) = name ++ " <" ++ showRoutedEmailAddress mail ++ ">"
showMailbox (Mailbox Nothing mail) = showRoutedEmailAddress mail

showRoutedEmailAddress (NormalEmailAddress mail) = showEmailAddress mail
showRoutedEmailAddress (RoutedEmailAddress _ mail) = showEmailAddress mail

showEmailAddress (EmailAddress local domain) = local ++ "@" ++ showDomain domain

showDomain (Domain s) = s
showDomain (LiteralDomain s) = s
