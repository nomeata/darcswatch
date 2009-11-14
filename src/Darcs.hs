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
	, getInventory
	, parseMail
	, patchBasename
	, inversePatch
	) where

import OldDate
import StringCrypto

import System.Time
import CachedGet
import Zip

import qualified Data.ByteString.Char8 as B
import Data.ByteString.Char8 (ByteString)
import Data.List

-- | The defining informtion of a Darcs patch.
data PatchInfo = PatchInfo
	{ piDate    :: ByteString
	, piName    :: ByteString
	, piAuthor  :: ByteString
	, piLog     :: [ByteString]
	, piInverted :: Bool
   } deriving (Eq,Ord,Show)


-- | Toggles the 'piInverted' flag of a 'PatchInfo'
inversePatch :: PatchInfo -> PatchInfo
inversePatch p@(PatchInfo {piInverted = i}) = p {piInverted = not i}

-- | Given a directory used for caching, and an URL of a Darcs repository,
--   it will return the list of patches in the repository, and whether the
--   repository as changed since the last run.
getInventory :: (String -> IO ()) -> FilePath -> String -> IO ([PatchInfo], Bool)
getInventory write cDir repo = do
	format <- get write False cDir formatUrl
	case format of
		Nothing                                     -> getInventory1 write cDir repo
		Just (f,_) | f == B.pack "hashed\ndarcs-2\n" -> getInventory2 write cDir repo
		           | f == B.pack "darcs-1.0\n"      -> getInventory1 write cDir repo
		           | f == B.pack "hashed\n"         -> getInventory2 write cDir repo
		           | otherwise                      -> error $ "Unkown repository format: " ++ B.unpack f ++ " in repo " ++ repo
  where	formatUrl = addSlash repo ++ "_darcs/format"

-- | Gets called when old style format was detected
getInventory1 :: (String -> IO ()) -> FilePath -> String -> IO ([PatchInfo],Bool)
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
	     (l1,r) | l1 == B.pack "Starting with tag:" -> do
	     	let p = head patches
		let filename = addSlash repo ++ "_darcs/inventories/" ++  patchBasename p ++ ".gz"
                (prev_p,prev_u) <- getInventoryFile True filename
		return (prev_p ++ patches, prev_u || updated)
	     _ -> return (patches, updated)

-- | Gets called when new style format was detected
getInventory2 :: (String -> IO ()) -> FilePath -> String -> IO ([PatchInfo],Bool)
getInventory2 write cDir repo = getInventoryFile False (addSlash repo ++ "_darcs/hashed_inventory")
  where maybe' m f d = maybe d f m
	getInventoryFile trustCache url = do
		inv <- get write trustCache cDir url
		maybe' inv parseBody $ do
			write $ "Repository " ++ repo ++ " not found.\n"
			return ([],False)
	skip_pristine s = case breakOn '\n' s of
	                    (l1,r) | B.pack "pristine" `B.isPrefixOf` l1 -> B.tail r
			    _                                            -> s
	parseStart body = do 
	   case breakOn '\n' (skip_pristine body) of
	     (l,r) | l == B.pack "Starting with inventory:" -> do
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
readPatchInfo s | B.null (dropWhite s) = Nothing
readPatchInfo s =
    if B.head (dropWhite s) /= '[' -- ]
    then Nothing
    else case breakOn '\n' $ B.tail $ dropWhite s of
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
breakOn c = B.break (c ==)

breakLast c p = case B.elemIndexEnd c p of
    Nothing -> Nothing
    Just n -> Just (B.take n p, B.drop (n+1) p)


-- | Given the content of a patch bundle, it returns a list of submitted patches with
--   their diff, and the list of patches in the context.
parseMail :: ByteString -> ([(PatchInfo,ByteString)],[PatchInfo])
parseMail content = do case scan_bundle content of 
			Left err -> ([],[])  -- putStrLn $ "Parse error: "++ err
			Right res -> if res == res then res else res

scan_bundle :: ByteString -> Either String ([(PatchInfo,ByteString)],[PatchInfo])
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
silly_lex ps = (B.unpack $ B.takeWhile (/='\n') $ dropWhite ps,
                           B.dropWhile (/='\n') $ dropWhite ps)

get_context :: ByteString -> ([PatchInfo],ByteString)
get_context ps =
    case readPatchInfo ps of
    Just (pinfo,r') -> pinfo -:- get_context r'
    Nothing -> ([],ps)

filter_gpg_dashes :: ByteString -> ByteString
filter_gpg_dashes ps =
    B.unlines $ map drop_dashes $
    takeWhile (/= B.pack "-----END PGP SIGNED MESSAGE-----") $
    dropWhile not_context_or_newpatches $ B.lines ps
    where drop_dashes x = if B.length x < 2 then x
                          else if B.take 2 x == B.pack "- "
                               then B.drop 2 x
                               else x
          not_context_or_newpatches s = (s /= B.pack "Context:") &&
                                        (s /= B.pack "New patches:")

readDiff :: ByteString -> Maybe (ByteString, ByteString)
readDiff s | B.null (dropWhite s) = Nothing
readDiff s = find (\(p,r) -> B.pack "\n\n" `B.isPrefixOf` r || B.pack "\n[" `B.isPrefixOf` r)
                  (zip (B.inits s) (B.tails s))

patchFilename :: PatchInfo -> String
patchFilename pi = patchBasename pi ++ ".gz"

-- | Given a patch, it calculates the name of the file that darcs usually
--   stores it in, without the ".gz" suffix.
patchBasename :: PatchInfo -> String
patchBasename pi = showIsoDateTime d++"-"++sha1_a++"-"++sha1 (B.unpack sha1_me)
        where b2ps True = B.pack "t"
              b2ps False = B.pack "f"
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
