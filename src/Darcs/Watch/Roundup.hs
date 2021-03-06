module Darcs.Watch.Roundup where

import Codec.MIME.String as MIME
import Data.List
import System.IO
import System.Exit
import System.Process
import Control.Monad

import Darcs.Watch.Data
import Darcs
import HTML

tellRoundup :: DarcsWatchConfig -> String -> RepositoryURL -> BundleHash -> PatchBundle -> BundleState -> IO ()
tellRoundup _      _   _    _          _      status | status `notElem` [Applied, Applicable] = return ()
tellRoundup _      url _    _          _      _      | not $ "http://bugs.darcs.net/" `isPrefixOf` url = return ()
tellRoundup _      _   repo _          _      status | repo == "http://darcs.net/screened" && status == Applicable = return () -- No applicable mails for the screened repo
tellRoundup config url repo bundleHash bundle status = do
	message <- flatten [mk_header ["From: " ++ from]
	                   ,mk_header ["To: " ++ to]
			   ,mk_header ["Subject: " ++ subject]
			   ] body Nothing []
	if cSendMails config
	  then do sendMail message
	  else do hPutStrLn stderr "Would send this message:"
	          hPutStrLn stderr message
  where from = cDarcsWatchAddress config
        to = "bugs@darcs.net"
	subject = case status of
	 	   Applicable -> "This patch is being tracked by DarcsWatch [" ++ roundupId ++ "] [darcswatchurl=" ++ bundleLink ++ "]"
		   Applied -> "This patch has been applied [" ++ roundupId ++ "] [status=" ++ roundupStatus ++ "]"
        roundupStatus | repo == "http://darcs.net/screened" = "needs-review"
                      | otherwise                           = "accepted"
	body = case status of
		Applicable -> "" -- no messages about this, please
		Applied ->    "This patch bundle (with " ++ show (length (fst bundle)) ++
			      " patches) was just applied to the repository " ++ repo ++".\n" ++
			      "This message was brought to you by " ++
			      "DarcsWatch\n" ++
			      bundleLink
	bundleLink = bundleURL config repo bundleHash
        numPatches = length (fst bundle)
	roundupId = drop (length "http://bugs.darcs.net/") url

sendMail ::  String -> IO ()
sendMail text = do
      (inh, outh, errh, ph) <- runInteractiveProcess "/usr/sbin/sendmail" ["-t"] Nothing Nothing
      hClose outh
      hPutStr inh text
      hClose inh
      err <- hGetContents errh
      hPutStr stderr err
      ec <- waitForProcess ph
      return ()
      

haveRoundupURL = msum . map fromRoundup 

fromRoundup (_,ViaBugtracker r,_) = Just r
fromRoundup  _                    = Nothing
