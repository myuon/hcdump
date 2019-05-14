module Data.StringBuffer (
  module StringBuffer,
  fromByteString,
  toByteString,
  dropWhile,
) where

import Prelude hiding (dropWhile)
import StringBuffer
import Data.ByteString.Internal (ByteString(..))

fromByteString :: ByteString -> StringBuffer
fromByteString (PS p l o) = StringBuffer p o l

toByteString :: StringBuffer -> ByteString
toByteString (StringBuffer p o l) = PS p l o

dropWhile :: (Char -> Bool) -> StringBuffer -> StringBuffer
dropWhile f buf =
  let (b, bs) = nextChar buf in if f b then dropWhile f bs else buf
