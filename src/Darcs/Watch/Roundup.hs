module Darcs.Watch.Roundup where

import Codec.MIME.String as MIME

import Darcs.Watch.Data
import Darcs
import HTML

tellRoundupAboutURL :: DarcsWatchConfig -> PatchBundle -> IO ()
tellRoundupAboutURL _ ([],_) = return ()
tellRoundupAboutURL config bundle = do
	message <- flatten [mk_header ["From: " ++ from]
	                   ,mk_header ["To: " ++ to]
			   ,mk_header ["Subject: " ++ subject]
			   ] body Nothing []
	if cSendRoundupMails config
	  then do fail "Not implemented yet"
	  else do putStrLn "Would send this message:"
	          putStrLn message
  where from = cDarcsWatchAddress config
        to = "SomeAdress@bugs.darcs.net"
	subject = "This patch is being tracked by DarcsWatch"
	body = "This patch bundle (with " ++ show (length (fst bundle)) ++" patches) is now " ++
	       "tracked on DarcsWatch <" ++ cDarcsWatchURL config ++
	       userFile (piAuthor (fst (head (fst bundle)))) ++ ">."
