module KnucleotideNew where

import Control.Concurrent.Async
import Data.IORef
import Data.Monoid
import Data.Bits
import Data.List
import Data.Word
import Data.Hashable
import Data.Traversable
import Text.Printf

import Data.Maybe
import Control.Monad

import qualified Data.Char as C
import qualified Data.ByteString.Char8 as B
import qualified Data.HashMap.Strict as M
import qualified Data.HashTable.ST.Cuckoo as STH
import qualified Control.Monad.ST as ST

{- By using only 2 bits to encode keys, it's important to use a different table
 - for different key sizes. Otherwise, if we encode 'A' as 0x00, "AT" and
 - "AAT" would map to the same bucket in the table.
 -
 - We could use 3 bits per letter to avoid this problem if needed.
 -}
bitsForChar :: Char -> Word64
bitsForChar 'a' = 0
bitsForChar 'A' = 0
bitsForChar 'c' = 1
bitsForChar 'C' = 1
bitsForChar 'g' = 2
bitsForChar 'G' = 2
bitsForChar 't' = 3
bitsForChar 'T' = 3
bitsForChar _   = error "Ay, Caramba!"

charForBits :: Word64 -> Char
charForBits 0 = 'A'
charForBits 1 = 'C'
charForBits 2 = 'G'
charForBits 3 = 'T'
charForBits _ = error "Ay, Caramba!"

packKey :: B.ByteString -> Word64
packKey = go zeroBits
  where
    go k bs = case B.uncons bs of
        Nothing      -> k
        Just (c, cs) -> go (unsafeShiftL k 2 .|. bitsForChar c) cs
{-# INLINE packKey #-}

unpackKey :: Int -> Word64 -> B.ByteString
unpackKey = go []
  where
    go s 0 _ = B.pack s
    go s l i = go (charForBits (i .&. 3) : s) (l-1) (unsafeShiftR i 2)
{-# INLINE unpackKey #-}

type CountMap = STH.HashTable ST.RealWorld Word64 Int

type CountMaps = M.HashMap Int CountMap

extractSequence :: String -> B.ByteString -> B.ByteString
extractSequence s = findSeq
  where
    prefix = B.pack ('>' : s)
    skipSeq =
          B.dropWhile (/= '>')
        . B.drop 1
    takeSeq =
          B.filter    (/= '\n')
        . B.takeWhile (/=  '>') -- extract until next header
        . B.dropWhile (/= '\n') -- skip header
    findSeq str
        | prefix `B.isPrefixOf` str  = takeSeq str
        | otherwise                  = findSeq (skipSeq str)

sortFreq :: [(String, Double)] -> [(String, Double)]
sortFreq = sortBy (\(k, v) (k', v') -> (compare v' v) `mappend` (compare k k'))

printFreq :: [(String, Double)] -> IO ()
printFreq l = forM_ (sortFreq $ fmap (\(s, d) -> (s, 100 * d)) l) $ uncurry (printf "%s %.3f\n")

singleLetters :: [String]
singleLetters = ["A", "C", "G", "T"]

doubleLetters :: [String]
doubleLetters = (++) <$> singleLetters <*> singleLetters

keys :: [String]
keys = ["GGT", "GGTA", "GGTATT", "GGTATTTTAATT", "GGTATTTTAATTTATAGT"]

segmentSizes :: [Int]
segmentSizes = [1, 2, 3, 4, 6, 12, 18]

shiftsFromSize :: Int -> [Int]
shiftsFromSize segmentSize = [0..segmentSize-1]

frequencies :: CountMaps -> Int -> [String] -> [(String, Double)]
frequencies countMaps segmentSize keys =
  let countMap = M.lookupDefault M.empty segmentSize countMaps
      totalCount = fromIntegral $ M.foldl' (+) 0 countMap
      frequencyPairs = fmap (\mapKey -> (mapKey, (fromIntegral $ M.lookupDefault 0 (packKey $ B.pack mapKey) countMap) / totalCount)) keys
  in  sortFreq frequencyPairs

searchFor :: IORef CountMap -> B.ByteString -> Int -> Int -> IO ()
searchFor countMap toSearchIn initialShift segmentSize =
  let bytesLength = B.length toSearchIn
      getSegment shift = packKey $ B.take segmentSize $ B.drop shift toSearchIn
      addIn shift = do
        let withinBytes = shift + segmentSize < bytesLength
        when withinBytes $ atomicModifyIORef' countMap (\cMap -> (M.alter (Just . (+1) . fromMaybe 0) (getSegment shift) cMap, ()))
        if withinBytes then addIn (shift + segmentSize) else return ()
  in  addIn initialShift

buildCountMap :: B.ByteString -> Int -> IO CountMap
buildCountMap toSearchIn segmentSize = do
  mapRef <- newIORef M.empty
  let processSearch = searchFor mapRef toSearchIn
  mapConcurrently_ (\shift -> processSearch shift segmentSize) $ shiftsFromSize segmentSize
  readIORef mapRef

main :: IO ()
main = do
  toSearchIn <- extractSequence "THREE" <$> B.getContents
  countMapsList <- mapConcurrently (\segmentSize -> fmap (\countMap -> (segmentSize, countMap)) $ buildCountMap toSearchIn segmentSize) segmentSizes
  let countMaps = M.fromList countMapsList

  printFreq (frequencies countMaps 1 singleLetters)
  putStrLn ""
  printFreq (frequencies countMaps 2 doubleLetters)
  putStrLn ""
  forM_ keys $ \k -> printf "%d\t%s\n" (M.lookupDefault 0 (packKey $ B.pack k) $ M.lookupDefault M.empty (length k) countMaps) k
