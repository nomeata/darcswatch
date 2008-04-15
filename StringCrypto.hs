
module StringCrypto 
	( md5
	, sha1
	) where

import qualified Data.Digest.SHA1 as SHA1
import qualified Data.Digest.MD5 as MD5
import Data.Word
import Data.Char

md5 :: String -> String
md5 = toHex1 . MD5.hash . map (fromIntegral. ord)

toHex1 :: [Word8] -> String
toHex1 = concatMap showAsHex1

showAsHex1 :: Word8 -> String
showAsHex1 n = showIt 2 n ""
   where
    showIt :: Int -> Word8 -> String -> String
    showIt 0 _ r = r
    showIt i x r = case quotRem x 16 of
                       (y, z) -> let c = intToDigit (fromIntegral z)
                                 in c `seq` showIt (i-1) y (c:r)



sha1 :: String -> String
sha1 = toHex2 . SHA1.hash . map (fromIntegral . ord)

toHex2 :: SHA1.Word160 -> String
toHex2 (SHA1.Word160 a b c d e)
 = concatMap showAsHex2 [a, b, c, d, e]

showAsHex2 :: Word32 -> String
showAsHex2 n = showIt 8 n ""
   where
    showIt :: Int -> Word32 -> String -> String
    showIt 0 _ r = r
    showIt i x r = case quotRem x 16 of
                       (y, z) -> let c = intToDigit (fromIntegral z)
                                 in c `seq` showIt (i-1) y (c:r)



