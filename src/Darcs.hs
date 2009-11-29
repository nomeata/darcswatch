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

module Darcs
	( PatchInfo(..)
	, PatchBundle
	, getInventory
	, parseMail
	, patchBasename
	, inversePatch
	, make_bundle
	, scan_bundle
	, hash_bundle
	, make_context
	, scan_context
	) where

import OldDate
import StringCrypto

import System.Time
import CachedGet
import Zip

import Printer
import SHA1

import Control.Arrow

import qualified Data.ByteString.Char8 as B
import Data.ByteString.Char8 (ByteString)
import Data.List

import Darcs.Watch.Data


-- | The defining informtion of a Darcs patch.
data PatchInfo = PatchInfo
	{ piDate    :: ByteString
	, piName    :: ByteString
	, piAuthor  :: ByteString
	, piLog     :: [ByteString]
	, piInverted :: Bool
   } deriving (Eq,Ord,Show)

-- | A patch bundle (e.g. a mail)
type PatchBundle = ([(PatchInfo,ByteString)],[PatchInfo])

-- | Toggles the 'piInverted' flag of a 'PatchInfo'
inversePatch :: PatchInfo -> PatchInfo
inversePatch p@(PatchInfo {piInverted = i}) = p {piInverted = not i}

-- | Given a directory used for caching, and an URL of a Darcs repository,
--   it will return the list of patches in the repository, and whether the
--   repository as changed since the last run.
getInventory :: (String -> IO ()) -> FilePath -> RepositoryURL -> IO ([PatchInfo], Bool)
getInventory write cDir repo = do
	format <- get write False cDir formatUrl
	case format of
		Nothing                           -> getInventory1 write cDir repo
		Just (f,_) | f == litHashedDarcs2 -> getInventory2 write cDir repo
		           | f == litDarcs10      -> getInventory1 write cDir repo
		           | f == litHashed       -> getInventory2 write cDir repo
		           | otherwise            -> error $ "Unkown repository format: " ++ B.unpack f ++ " in repo " ++ repo
  where	formatUrl = addSlash repo ++ "_darcs/format"

-- | Gets called when old style format was detected
getInventory1 :: (String -> IO ()) -> FilePath -> RepositoryURL -> IO ([PatchInfo],Bool)
getInventory1 write cDir repo = getInventoryFile False (addSlash repo ++ "_darcs/inventory")
  where maybe' m f d = maybe d f m
	getInventoryFile trustCache url = do
		inv <- get write trustCache cDir url
		maybe' inv parseBody $ do
			write $ "Repository " ++ repo ++ " not found.\n"
			return ([],False)
	parseBody (body, updated) = do
	   let unzipped = maybeUnzipB body
	   let patches = readPatchInfos unzipped
	   case breakOn '\n' unzipped of
	     (l1,r) | l1 == litStartingWithTag -> do
	     	let p = head patches
		let filename = addSlash repo ++ "_darcs/inventories/" ++  patchBasename p ++ ".gz"
                (prev_p,prev_u) <- getInventoryFile True filename
		return (prev_p ++ patches, prev_u || updated)
	     _ -> return (patches, updated)

-- | Gets called when new style format was detected
getInventory2 :: (String -> IO ()) -> FilePath -> RepositoryURL -> IO ([PatchInfo],Bool)
getInventory2 write cDir repo = getInventoryFile False (addSlash repo ++ "_darcs/hashed_inventory")
  where maybe' m f d = maybe d f m
	getInventoryFile trustCache url = do
		inv <- get write trustCache cDir url
		maybe' inv parseBody $ do
			write $ "Repository " ++ repo ++ " not found.\n"
			return ([],False)
	skip_pristine s = case breakOn '\n' s of
	                    (l1,r) | litPristine `B.isPrefixOf` l1 -> B.tail r
			    _                                            -> s
	parseStart body = do 
	   case breakOn '\n' (skip_pristine body) of
	     (l,r) | l == litStartingWithInventory -> do
	     	 case breakOn '\n' $ B.tail r of
		   (h,r'') -> do prev <- getInventoryFile True (addSlash repo ++ "_darcs/inventories/" ++ B.unpack h)
		                 return (prev,B.tail r'')
	           --_ -> putStrLn "Broken inventory start line" >> return (([],False),body)
             _ -> return (([],False),body)
	parseBody (body, updated) = do
	   let unzipped = maybeUnzipB body
	   ((prev_patches, prev_updated),body') <- parseStart unzipped
           return (prev_patches ++ readPatchInfos unzipped, prev_updated || updated)
	  


readPatchInfos :: ByteString -> [PatchInfo]
readPatchInfos inv | B.null inv = []
readPatchInfos inv = case breakOn '[' inv of
			(_,r) -> case readPatchInfo r of
			     Just (pinfo,r) -> pinfo : readPatchInfos r
			     Nothing -> []

readPatchInfo :: ByteString -> Maybe (PatchInfo, ByteString)
readPatchInfo s =
    if B.null s' || B.head s' /= '[' -- ]
    then Nothing
    else case breakOn '\n' (B.tail s') of
         (name,s') | B.null s' -> error $ "Broken file (1) " ++ show (B.unpack s)
         (name,s') | otherwise -> 
             case breakOn '*' $ B.tail s' of
             (author,s2) | B.null s2 -> error "Broken file (2)"
	                 | otherwise -> 
                 case B.break (\c->c==']'||c=='\n') $ B.drop 2 s2 of
                 (ct,s''') ->
                     do (log, s4) <- lines_starting_with_ending_with ' ' ']' $ dn s'''
                        let not_star = B.index s2 1 /= '*'
                        return $ (PatchInfo { piDate = ct
                                            , piName = name
                                            , piAuthor = author
                                            , piLog = log
                                            , piInverted = not_star
                                            }, s4)
    where dn x = if B.null x || B.head x /= '\n' then x else B.tail x
    	  s' = dropWhite s

lines_starting_with_ending_with :: Char -> Char -> ByteString -> Maybe ([ByteString],ByteString)
lines_starting_with_ending_with st en s = lswew s
    where
  lswew x | B.null x = Nothing
  lswew x =
    if B.head x == en
    then Just ([], B.tail x)
    else if B.head x /= st
         then Nothing
         else case breakOn '\n' $ B.tail x of
              (l,r) -> case lswew $ B.tail r of
                       Just (ls,r') -> Just (l:ls,r')
                       Nothing ->
                           case breakLast en l of
                           Just (l2,_) ->
                               Just ([l2],  B.drop (B.length l2+2) x)
                           Nothing -> Nothing


dropWhite = B.dropWhile (`elem` " \n\t\r")
breakOn :: Char -> ByteString -> (ByteString, ByteString)
breakOn c = B.break (==c)

breakLast c p = case B.elemIndexEnd c p of
    Nothing -> Nothing
    Just n -> Just (B.take n p, B.drop (n+1) p)

showPatchInfo :: PatchInfo -> Doc
showPatchInfo pi =
    blueText "[" <> packedString (piName pi)
 $$ packedString (piAuthor pi) <> text inverted <> packedString (piDate pi)
                                 <> myunlines (piLog pi) <> blueText "] "
    where inverted = if piInverted pi then "*-" else "**"
          myunlines [] = empty
          myunlines xs = mul xs
              where mul [] = text "\n"
                    mul (s:ss) = text "\n " <> packedString s <> mul ss

-- | Given the content of a patch bundle, it returns a list of submitted patches with
--   their diff, and the list of patches in the context.
parseMail :: ByteString -> PatchBundle
parseMail content = do case scan_bundle content of 
			Left err -> ([],[])  -- putStrLn $ "Parse error: "++ err
			Right res -> if res == res then res else res

showPatch :: (PatchInfo,ByteString) -> Doc
showPatch (pi,d) = showPatchInfo pi <> packedString d

make_bundle :: PatchBundle -> ByteString
make_bundle bundle@(to_be_sent, common) = renderPS $
                           text ""
                           $$ text "New patches:"
                           $$ text ""
                           $$ (vsep $ map showPatch to_be_sent)
                           $$ text ""
                           $$ text "Context:"
                           $$ text ""
                           $$ (vcat $ map showPatchInfo common)
                           $$ text "Patch bundle hash:"
                           $$ text (hash_bundle bundle)
                           $$ text ""

hash_bundle :: PatchBundle -> String
hash_bundle (to_be_sent,_) = sha1PS $ renderPS $ vcat (map showPatch to_be_sent) <> newline


scan_bundle :: ByteString -> Either String PatchBundle
scan_bundle ps
  | B.null ps = Left "Bad patch bundle!"
  | otherwise =
    case silly_lex ps of
    ("New patches:",rest) ->
        case get_patches rest of
        (submitted, rest') ->
            case silly_lex rest' of
            ("Context:", rest'') ->
                case get_context rest'' of
                (context,maybe_hash) -> -- FIXME verify patch bundle hash
                    Right (submitted, context)
            (a,r) -> Left $ "Malformed patch bundle: '"++a++"' is not 'Context:'"
                     ++ "\n" ++  B.unpack r
    ("Context:",rest) ->
        case get_context rest of
        (context, rest') ->
            case silly_lex rest' of
            ("New patches:", rest'') ->
                case get_patches rest'' of
                (submitted,_) -> Right (submitted, context)
            (a,_) -> Left $ "Malformed patch bundle: '" ++ a ++ "' is not 'New patches:'"
    ("-----BEGIN PGP SIGNED MESSAGE-----",rest) ->
            scan_bundle $ filter_gpg_dashes rest
    (_,rest) -> scan_bundle rest

get_patches :: ByteString -> ([(PatchInfo,ByteString)], ByteString)
get_patches ps = 
    case readPatchInfo ps of
    Nothing -> ([], ps)
    Just (pinfo,ps) ->
         case readDiff ps of
         Nothing -> ([], ps)
         Just (diff, r) -> (pinfo, diff) -:- get_patches r


silly_lex :: ByteString -> (String, ByteString)
silly_lex = first B.unpack . B.span (/='\n') . dropWhite

make_context :: [PatchInfo] -> ByteString
make_context = renderPS . vcat . map showPatchInfo


scan_context :: ByteString -> [PatchInfo]
scan_context = fst . get_context

get_context :: ByteString -> ([PatchInfo],ByteString)
get_context ps =
    case readPatchInfo ps of
    Just (pinfo,r') -> pinfo -:- get_context r'
    Nothing -> ([],ps)

filter_gpg_dashes :: ByteString -> ByteString
filter_gpg_dashes ps =
    B.unlines $ map drop_dashes $
    takeWhile (/= litEndPGPSignedMessages) $
    dropWhile not_context_or_newpatches $ B.lines ps
    where drop_dashes x = if B.length x < 2 then x
                          else if B.take 2 x == litDashSpace
                               then B.drop 2 x
                               else x
          not_context_or_newpatches s = (s /= litContext) &&
                                        (s /= litNewPatches)

readDiff :: ByteString -> Maybe (ByteString, ByteString)
readDiff s = 
	if B.null s' then Nothing
	else find (\(p,r) -> litNewlineNewline `B.isPrefixOf` r
 	                  || litNewlineBracket `B.isPrefixOf` r)
                  (zip (B.inits s') (B.tails s'))
  where	s' = dropWhite s

patchFilename :: PatchInfo -> String
patchFilename pi = patchBasename pi ++ ".gz"

-- | Given a patch, it calculates the name of the file that darcs usually
--   stores it in, without the ".gz" suffix.
patchBasename :: PatchInfo -> String
patchBasename pi = showIsoDateTime d++"-"++sha1_a++"-"++sha1 (B.unpack sha1_me)
        where b2ps True  = litT
	      b2ps False = litF
              sha1_me = B.concat [piName pi,
                                  piAuthor pi,
                                  piDate pi,
                                  B.concat $ piLog pi,
                                  b2ps $ piInverted pi]
              d = readPatchDate $ B.unpack $ piDate pi
              sha1_a = take 5 $ sha1 $ B.unpack $ piAuthor pi

readPatchDate :: String -> CalendarTime
readPatchDate = ignoreTz . readUTCDate
  where ignoreTz ct = ct { ctTZ = 0 }


(-:-) :: a -> ([a],b)  -> ([a],b)
a -:- (as,r) = (a:as,r)

addSlash filename | last filename == '/' = filename
                  | otherwise            = filename ++ "/"

-- Packed bytestring literators, to avoid re-packing them constantly (ghc is
-- probably not smart enough to do it by itself 

litHashedDarcs2 = B.pack "hashed\ndarcs-2\n"
litDarcs10 = B.pack "darcs-1.0\n"
litHashed = B.pack "hashed\n"
litStartingWithTag = B.pack "Starting with tag:"
litPristine = B.pack "pristine"
litStartingWithInventory = B.pack "Starting with inventory:"
litEndPGPSignedMessages = B.pack "-----END PGP SIGNED MESSAGE-----"
litDashSpace = B.pack "- "
litContext = B.pack "Context:"
litNewPatches = B.pack "New patches:"
litNewlineNewline = B.pack "\n\n" 
litNewlineBracket = B.pack "\n[" 
litT = B.pack "t"
litF = B.pack "f"


