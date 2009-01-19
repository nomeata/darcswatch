module Zip (unzipB, maybeUnzipB, maybeUnzipLB) where

import qualified Data.ByteString.Char8 as B
import Data.ByteString.Char8 (ByteString)

import qualified Data.ByteString.Lazy.Char8 as L

import Codec.Compression.GZip


unzipB :: ByteString -> ByteString
unzipB = B.concat . L.toChunks . decompress . L.fromChunks . (:[])

maybeUnzipLB :: L.ByteString -> L.ByteString
maybeUnzipLB s = if L.pack "\US\139" `L.isPrefixOf` s then decompress s  else s

maybeUnzipB :: ByteString -> ByteString
maybeUnzipB s = if B.pack "\US\139" `B.isPrefixOf` s then  unzipB s  else s
