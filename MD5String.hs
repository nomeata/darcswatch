
module MD5String 
	( md5
	) where

import MD5  hiding (md5)

md5 :: String -> String
md5 s = md5s (Str s)
