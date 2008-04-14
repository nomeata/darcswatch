module Darcs
	( parseInventory
	, parseMail
	, PatchInfo(..)
	) where


data PatchInfo = PatchInfo
	{ piDate :: String
	, piName    :: String
	, piAuthor  :: String
	, piLog     :: [String]
	, piInverted :: Bool
   } deriving (Eq,Ord,Show)


parseInventory :: String -> IO [PatchInfo]
parseInventory content' = do
	let (firstLine, rest) = breakOn '\n' content'
	let content = if firstLine == "Starting with tag:" then rest else content'
	let pis = readPatchInfos content
	putStrLn ("Found " ++ show (length (pis)) ++ " patches")
	return pis

readPatchInfos :: String -> [PatchInfo]
readPatchInfos inv | null inv = []
readPatchInfos inv = case readPatchInfo inv of
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
readPatch s =
    if head (dropWhite s) /= '{' -- ]
    then Nothing
    else do let l = lines $ dn s
    	        (want, r) = (takeWhile (/="}") l, dropWhile (/="}") l)
	    if null r
	      then Nothing
	      else return (unlines want ++ (head r), unlines (tail r))
    where dn x = if null x || head x /= '\n' then x else tail x

(-:-) :: a -> ([a],b)  -> ([a],b)
a -:- (as,r) = (a:as,r)

