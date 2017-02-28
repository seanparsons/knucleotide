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

type CountMap = M.HashMap B.ByteString Int

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
        | prefix `B.isPrefixOf` str  = B.pack $ fmap C.toUpper $ B.unpack $ takeSeq str
        | otherwise                  = findSeq (skipSeq str)

sortFreq :: [(String, Double)] -> [(String, Double)]
sortFreq = sortBy (\(k, v) (k', v') -> (compare v' v) `mappend` (compare k k'))

printFreq :: [(String, Double)] -> IO ()
printFreq l = forM_ (sortFreq $ fmap (\(s, d) -> (s, 100 * d)) l) $ uncurry (printf "%s %.3f\n")

keys :: [String]
keys = ["GGT", "GGTA", "GGTATT", "GGTATTTTAATT", "GGTATTTTAATTTATAGT"]

segmentSizes :: [Int]
segmentSizes = [1, 2, 3, 4, 6, 12, 18]

sizesAndShifts :: [(Int, Int)]
sizesAndShifts = do
  segmentSize <- segmentSizes
  shift <- [0..segmentSize-1]
  return (segmentSize, shift)

frequencies :: CountMap -> Int -> [(String, Double)]
frequencies countMap segmentSize =
  let filtered = filter (\(mapKey, _) -> B.length mapKey == segmentSize) $ M.toList countMap
      totalCount = fromIntegral $ getSum $ foldMap (Sum . snd) filtered
      frequencyPairs = fmap (\(mapKey, keyCount) -> (B.unpack mapKey, (fromIntegral keyCount) / totalCount)) filtered
  in  sortFreq frequencyPairs

searchFor :: IORef CountMap -> B.ByteString -> Int -> Int -> IO ()
searchFor countMap toSearchIn initialShift segmentSize =
  let bytesLength = B.length toSearchIn
      getSegment shift = B.take segmentSize $ B.drop shift toSearchIn
      addIn shift = do
        let withinBytes = shift + segmentSize < bytesLength
        when withinBytes $ atomicModifyIORef' countMap (\cMap -> (M.alter (Just . (+1) . fromMaybe 0) (getSegment shift) cMap, ()))
        if withinBytes then addIn (shift + segmentSize) else return ()
  in  addIn initialShift

main :: IO ()
main = do
  toSearchIn <- extractSequence "THREE" <$> B.getContents
  mapRef <- newIORef M.empty
  let processSearch = searchFor mapRef toSearchIn
  mapConcurrently_ (\(segmentSize, shift) -> processSearch shift segmentSize) sizesAndShifts
  countMap <- readIORef mapRef

  printFreq (frequencies countMap 1)
  putStrLn ""
  printFreq (frequencies countMap 2)
  putStrLn ""
  forM_ keys $ \k -> printf "%d\t%s\n" (M.lookupDefault 0 (B.pack k) countMap) k
