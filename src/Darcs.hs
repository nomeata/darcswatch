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

-- | The defining informtion of a Darcs patch.
data PatchInfo = PatchInfo
	{ piDate :: String
	, piName    :: String
	, piAuthor  :: String
	, piLog     :: [String]
	, piInverted :: Bool
   } deriving (Eq,Ord,Show)


-- | Toggles the 'piInverted' flag of a 'PatchInfo'
inversePatch :: PatchInfo -> PatchInfo
inversePatch p@(PatchInfo {piInverted = i}) = p {piInverted = not i}

-- | Given a directory used for caching, and an URL of a Darcs repository,
--   it will return the list of patches in the repository, and whether the
--   repository as changed since the last run.
getInventory :: FilePath -> String -> IO ([PatchInfo], Bool)
getInventory cDir repo = do
		old_inv <- get cDir old_inventory
		maybe' old_inv parseReturn $ do

			new_inv <- get cDir hashed_inventory
			maybe' new_inv parseReturn $ do

				putStrLn $ "Repository " ++ repo ++ " not found."
				return ([],False)

  where	old_inventory = addSlash repo ++ "_darcs/inventory"
        hashed_inventory = addSlash repo ++ "_darcs/hashed_inventory"
	maybe' m f d = maybe d f m
	parseReturn (body, updated) = return (readPatchInfos body, updated)

readPatchInfos :: String -> [PatchInfo]
readPatchInfos inv | null inv = []
readPatchInfos inv = case breakOn '[' inv of
			(_,r) -> case readPatchInfo r of
			     Just (pinfo,r) -> pinfo : readPatchInfos r
			     Nothing -> []

readPatchInfo :: String -> Maybe (PatchInfo, String)
readPatchInfo s | null (dropWhite s) = Nothing
readPatchInfo s =
    if head (dropWhite s) /= '[' -- ]
    then Nothing
    else case breakOn '\n' $ tail $ dropWhite s of
         (name,s') ->
             case breakOn '*' $ tail s' of
             (author,s2) ->
                 case break (\c->c==']'||c=='\n') $ drop 2 s2 of
                 (ct,s''') ->
                     do (log, s4) <- lines_starting_with_ending_with ' ' ']' $ dn s'''
                        let not_star = index s2 1 /= '*'
                        return $ (PatchInfo { piDate = ct
                                            , piName = name
                                            , piAuthor = author
                                            , piLog = log
                                            , piInverted = not_star
                                            }, s4)
    where dn x = if null x || head x /= '\n' then x else tail x

lines_starting_with_ending_with :: Char -> Char -> String -> Maybe ([String],String)
lines_starting_with_ending_with st en s = lswew s
    where
  lswew x | null x = Nothing
  lswew x =
    if head x == en
    then Just ([], tail x)
    else if head x /= st
         then Nothing
         else case breakOn '\n' $ tail x of
              (l,r) -> case lswew $ tail r of
                       Just (ls,r') -> Just (l:ls,r')
                       Nothing ->
                           case breakLast en l of
                           Just (l2,_) ->
                               Just ([l2], drop (length l2+2) x)
                           Nothing -> Nothing


dropWhite = dropWhile (`elem` " \n\t\r")
breakOn c = break (c ==)
breakFirst c xs = case breakOn c xs of
		      (ys, zs)
		       | null zs -> Nothing
		       | otherwise -> Just (ys, tail zs)

breakLast c xs = case breakFirst c (reverse xs) of
		     Nothing -> Nothing
		     Just (ys, zs) ->
                                 Just (reverse zs, reverse ys)
index = (!!)


-- | Given the content of a patch bundle, it returns a list of submitted patches with
--   their diff, and the list of patches in the context.
parseMail :: String -> ([(PatchInfo,String)],[PatchInfo])
parseMail content = do case eesc of 
			Left err -> ([],[])  -- putStrLn $ "Parse error: "++ err
			Right res -> if res == res then res else res
  where demime = readMail content
	eesc = scan_bundle demime
	

readMail :: String -> String
readMail s = s
--     We already strip the relevant part in the mail filter
--
--     case betweenLines
--          ("Content-Description: A darcs patch for your repository!")
--          ("--=_--") s of
--     Nothing -> s -- if it wasn't an email in the first place, just pass along.
--     Just s' -> qpdecode s'

qpdecode :: String -> String
qpdecode s = s -- FIXME

betweenLines :: String -> String -> String -> Maybe (String)
betweenLines start end s
 = case break (start ==) (lines s) of
	(_, _:rest) ->
       		case break (end ==) (reverse rest) of
			(_,_:rres) -> Just (unlines (reverse rres))
			_ -> Nothing
	_ -> Nothing

scan_bundle :: String -> Either String ([(PatchInfo,String)],[PatchInfo])
scan_bundle ps
  | null ps = Left "Bad patch bundle!"
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
                     ++ "\n" ++  r
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

get_patches :: String -> ([(PatchInfo,String)], String)
get_patches ps = 
    case readPatchInfo ps of
    Nothing -> ([], ps)
    Just (pinfo,ps) ->
         case readPatch ps of
         Nothing -> ([], ps)
         Just (patch, r) -> (pinfo, patch) -:- get_patches r


silly_lex :: String -> (String, String)
silly_lex ps = (takeWhile (/='\n') $ dropWhite ps,
                dropWhile (/='\n') $ dropWhite ps)

get_context :: String -> ([PatchInfo],String)
get_context ps =
    case readPatchInfo ps of
    Just (pinfo,r') -> pinfo -:- get_context r'
    Nothing -> ([],ps)

filter_gpg_dashes :: String -> String
filter_gpg_dashes ps =
    unlines $ map drop_dashes $
    takeWhile (/= "-----END PGP SIGNED MESSAGE-----") $
    dropWhile not_context_or_newpatches $ lines ps
    where drop_dashes x = if length x < 2 then x
                          else if take 2 x == "- "
                               then drop 2 x
                               else x
          not_context_or_newpatches s = (s /= "Context:") &&
                                        (s /= "New patches:")

readPatch :: String -> Maybe (String, String)
readPatch s | null (dropWhite s) = Nothing
readPatch s = if null r then Nothing else Just (unlines p,unlines r)
  where (p,r) = break (\l -> null l || head l == '[') (lines s)

patchFilename :: PatchInfo -> String
patchFilename pi = patchBasename pi ++ ".gz"

-- | Given a patch, it calculates the name of the file that darcs usually
--   stores it in, without the ".gz" suffix.
patchBasename :: PatchInfo -> String
patchBasename pi = showIsoDateTime d++"-"++sha1_a++"-"++sha1 sha1_me
        where b2ps True = "t"
              b2ps False = "f"
              sha1_me = concat   [piName pi,
                                  piAuthor pi,
                                  piDate pi,
                                  concat $ piLog pi,
                                  b2ps $ piInverted pi]
              d = readPatchDate $ piDate pi
              sha1_a = take 5 $ sha1 $ piAuthor pi

readPatchDate :: String -> CalendarTime
readPatchDate = ignoreTz . readUTCDate
  where ignoreTz ct = ct { ctTZ = 0 }


(-:-) :: a -> ([a],b)  -> ([a],b)
a -:- (as,r) = (a:as,r)

addSlash filename | last filename == '/' = filename
                  | otherwise            = filename ++ "/"
