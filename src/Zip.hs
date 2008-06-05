module Zip (unzipB, maybeUnzipB) where

import qualified Data.ByteString.Char8 as B
import Data.ByteString.Char8 (ByteString)

import qualified Data.ByteString.Lazy as L

import Codec.Compression.GZip


unzipB :: ByteString -> ByteString
unzipB = B.concat . L.toChunks . decompress . L.fromChunks . (:[])

maybeUnzipB :: ByteString -> ByteString
maybeUnzipB s = if B.pack "\US\139" `B.isPrefixOf` s then  unzipB s  else s
