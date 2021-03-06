module Darcs.Watch.Mail where

import Codec.MIME.String as MIME
import Data.List
import System.IO
import System.Exit
import System.Process
import Control.Monad
import Data.Maybe
import Data.Char
import qualified Data.ByteString.Char8 as B

import Darcs.Watch.Data
import Darcs
import HTML

mailSubscriptions :: DarcsWatchConfig -> RepositoryURL -> BundleHash -> PatchBundle -> [BundleHistory] -> BundleState -> IO ()
mailSubscriptions _      _    _          _      _       status
	| status `notElem` [Applied] = return ()
mailSubscriptions config repo bundleHash bundle history status = 
	forM_ (map snd (filter (\(r,m) -> r == repo) (cRepoSubscriptions config))) $ \to -> do
		message <- flatten [mk_header ["From: " ++ from]
				   ,mk_header ["To: " ++ to]
				   ,mk_header ["References: " ++ unwords references]
				   ,mk_header ["Subject: " ++ subject]
				   ] body Nothing []
		if cSendMails config
		  then do sendMail message
		  else do hPutStrLn stderr "Would send this message:"
			  hPutStrLn stderr message
  where from = cDarcsWatchAddress config
	subject = case status of
		   Applied -> case mapMaybe extractSubject history of 
			[] -> "Applied: Patch bundle"
		   	sj -> "Applied: " ++ fixUpSubject (last sj)
	body = case status of
		Applied ->    "This " ++
			       (if length (fst bundle) ==  1
			       	then "1-patch"
				else show (length (fst bundle)) ++ "-patches") ++ 
			       " bundle was just applied to " ++ repo ++
			      ":\n\n" ++
			      concatMap showPatch (fst bundle) ++
			      "\n-- \n" ++
			      "This message was brought to you by DarcsWatch\n" ++
			      bundleURL config repo bundleHash
        numPatches = length (fst bundle)
	references = mapMaybe extractMessageId history
	showPatch (pi,_) =
		B.unpack (piDate pi) ++ "  " ++ B.unpack (piAuthor pi) ++ "\n" ++
		" * " ++ B.unpack (piName pi) ++ "\n" ++
		unlines (map ((' ':). B.unpack) (stripIgnorethis (piLog pi))) ++
		"\n"

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

extractMessageId (_,ViaEMail _ _ _ (Just mid),_) = Just mid
extractMessageId  _                              = Nothing

extractSubject (_,ViaEMail _ _ subject _,_) = Just subject
extractSubject _                            = Nothing

fixUpSubject = dropWhile isSpace . dropBrackets . dropWhile isSpace

dropBrackets ('[':xs) = case dropWhile (/=']') xs of { "" -> ""; r -> tail r}
dropBrackets xs     = xs
