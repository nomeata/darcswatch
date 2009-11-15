module Darcs.Watch.Roundup where

import Codec.MIME.String as MIME
import Data.List
import System.IO
import System.Exit
import System.Process

import Darcs.Watch.Data
import Darcs
import HTML

tellRoundupAboutURL :: DarcsWatchConfig -> String -> PatchBundle -> IO ()
tellRoundupAboutURL _ _ ([],_) = return ()
tellRoundupAboutURL _ url _ | not $ "http://bugs.darcs.net/" `isPrefixOf` url = return ()
tellRoundupAboutURL config url bundle = do
	message <- flatten [mk_header ["From: " ++ from]
	                   ,mk_header ["To: " ++ to]
			   ,mk_header ["Subject: " ++ subject]
			   ] body Nothing []
	if cSendRoundupMails config
	  then do sendMail message
	  else do putStrLn "Would send this message:"
	          putStrLn message
  where from = cDarcsWatchAddress config
        to = "patches@bugs.darcs.net"
	subject = "This patch is being tracked by DarcsWatch [" ++ roundupId ++ "]"
	body = "This patch bundle (with " ++ show (length (fst bundle)) ++" patches) is now " ++
	       "tracked on DarcsWatch <" ++ cDarcsWatchURL config ++
	       userFile (piAuthor (fst (head (fst bundle)))) ++ ">."
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
      

