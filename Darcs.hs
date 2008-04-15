module Darcs
	( getInventory
	, parseInventory
	, parseMail
	, PatchInfo(..)
	, patchBasename
	) where

import OldDate
import StringCrypto

import System.Time
import Network.HTTP
import Network.URI

data PatchInfo = PatchInfo
	{ piDate :: String
	, piName    :: String
	, piAuthor  :: String
	, piLog     :: [String]
	, piInverted :: Bool
   } deriving (Eq,Ord,Show)



getInventory :: String -> IO [PatchInfo]
getInventory repo = do
		old_inv <- httpGet old_inventory
		maybe' old_inv (return . parseInventory) $ do

			new_inv <- httpGet hashed_inventory
			maybe' new_inv (return . parseInventory) $ do

				putStrLn $ "Repository " ++ repo ++ " not found."
				return []

  where	old_inventory = addSlash repo ++ "_darcs/inventory"
        hashed_inventory = addSlash repo ++ "_darcs/hashed_inventory"
	maybe' m f d = maybe d f m

httpGet uri' = do
	let Just uri = parseURI uri'
	result <- simpleHTTP (Request
		{ rqURI = uri
		, rqMethod = GET
		, rqHeaders = []
		, rqBody = ""
		})
	return $ case result of
		Left err -> Nothing
		Right response -> case rspCode response of 
			(2,_,_) -> Just $ rspBody response
			_       -> Nothing

parseInventory :: String -> [PatchInfo]
parseInventory content = readPatchInfos content

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


parseMail :: FilePath -> IO ([(PatchInfo,String)],[PatchInfo])
parseMail file = do 
	content <- readFile file
	let demime = readMail content
	-- verify here
	let eesc = scan_bundle demime
	case eesc of 
		Left err -> do putStrLn $ "Parse error: "++ err
		               return ([],[])
		Right res -> return res


	

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
