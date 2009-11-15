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

import Darcs.Watch.Data
import Darcs.Watch.Storage

main = do
	args <- getArgs
        let (confdir) = case args of
                        [confdir] -> (confdir)
                        _         -> error "Use convert confdir/ and pipe mail to it"
        config <- read `fmap` readFile (confdir </> "config")

	mail <- getContents
	let message = MIME.parse mail
	let mpatch = findDarcsBundle message
	case mpatch of
		Nothing -> return ()
		Just bundleData -> do
			let mi = m_message_info message
			    from = maybe "" (showFrom) (mi_from mi)
			    to   = maybe "" (showTo) (mi_to mi)
			    subject = maybe "" (\(Subject s) -> s) (mi_subject mi)
			    mid = get_header (mi_headers mi) "message-id:" Just 
			let Right bundle = scan_bundle (B.pack bundleData)
			bhash <- addBundle (cData config) bundle
			changeBundleState (cData config) bhash
				(ViaEMail from to subject mid) New

findDarcsBundle :: Message -> Maybe String
findDarcsBundle (Message _ _ (Data _ (ContentType "text" "x-darcs-patch" _) _ msg)) = Just msg
findDarcsBundle (Message _ _ (Body (ContentType "text" "x-darcs-patch" _) _ msg)) = Just msg
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

