import System.IO
import Data.List.Split
import Control.Applicative
import Data.Char 

import qualified PGM 
import qualified Data.Set as Set
import qualified Data.Map as Map
import qualified Data.List as List
import qualified Data.Ratio as Ratio
import qualified Data.ByteString.Lazy as L

import Data.Char (isSpace)
import Data.Ratio ((%))
import Data.Word (Word8)
import Data.List.Split (chunksOf)
import Data.Function (on)
import System.IO

data Bits a = One a | Zero a deriving (Eq,Show)
data Parity a = Odd a | Even a deriving (Eq,Show)

instance Functor Bits where  
    fmap f (One x) = One (f x)
    fmap f (Zero x) = Zero (f x)


main = do 
        putStr  "Enter a filename: "
        hFlush stdout
        fileName <- getLine
        pgmFileStream <- L.readFile fileName
        maybePgm <- return $ PGM.parse pgmFileStream
        case maybePgm of 
          Nothing -> putStrLn "Failed to parse"
          Just (pgm,rest) -> do
                               let newData = L.map threshold (PGM.bytes pgm)
                               let newPGM = PGM.PGM{ PGM.pgmType = (PGM.pgmType pgm),
                                                     PGM.comments = (PGM.comments pgm),
                                                     PGM.width = (PGM.width pgm),
                                                     PGM.height = (PGM.height pgm),
                                                     PGM.maxPixel = (PGM.maxPixel pgm),
                                                     PGM.bytes = newData }
                               estimate <- decode newPGM
                               print estimate


decode pgm = do
               let centerRow = (PGM.height pgm) `div` 2
               let rawRow = takeRow pgm centerRow
               let maybeRow = stripMarkers . dropEnclosingWhiteSpace . rle . (fmap toBinary) . L.unpack $ rawRow
               case maybeRow of
                 Nothing -> return ([])
                 Just row ->  do
                                let frontChunked = chunksOf 4 (frontHalf row)
                                let frontNormed = fmap normalize frontChunked
                                let backChunked = chunksOf 4 (backHalf row)
                                let backNormed = fmap normalize backChunked
                                let fe = fmap (fst . fst . closestMatchFront) frontNormed 
                                let be = fmap (fst . fst . closestMatchBack) backNormed 
                                let ae = fe ++ be
                                let firstNum = (fst . getFirst) fe
                                let result = [firstNum] ++ (fmap value ae)
                                return (result)

getFirst fe  = 
  let dist' as bs = foldl (\acc (a,b) -> if (a == b) then acc  else acc + 1 ) 0 (zip as bs)
      front = fmap (\x -> case x of 
                            Odd _ -> 'L'
                            Even _ -> 'G') fe
      dists = fmap (dist' front) fcodes
      sorted = List.sortBy (compare `on` snd) (zip [0..9] dists)

  in head sorted

closestMatchBack chunk =
  let evens = zip [Even d | d<-[0..9] ] [ normalize r  | r <- (fmap rle rcodes) ]
      dists = fmap (distance chunk) (fmap snd evens)
      sorted = List.sortBy (compare `on` snd) (zip evens dists)
  in head sorted

closestMatchFront chunk = 
  let odds =  zip [Odd d  | d<-[0..9] ] [ normalize l  | l <- (fmap rle lcodes) ]
      evens = zip [Even d | d<-[0..9] ] [ normalize r  | r <- (fmap rle gcodes) ]
      both = odds ++ evens
      dists = fmap (distance chunk) (fmap snd both)
      sorted = List.sortBy (compare `on` snd) (zip both dists)
  in head sorted


value a = case a of 
  Odd x -> x
  Even x -> x


distance as bs = sum $ zipWith bitsDistance as bs


bitsDistance (One a)  (Zero b) = 1 
bitsDistance (Zero a) (One b)  = 1 
bitsDistance (One a)  (One b)  = abs (a - b)
bitsDistance (Zero a) (Zero b) = abs (a - b)


normalize xs = fmap (\b -> fmap (\x-> x % _sum) b) xs 
               where _sum = sum $ fmap getValue xs


getValue :: Bits a -> a
getValue b = case b of
             One x -> x
             Zero x -> x

frontHalf xs = take (24) xs 

backHalf xs = take 24 $ drop (24 + 5) xs

stripMarkers xs = let frontMatches = and $ zipWith ($) [isOne, isZero, isOne] (take 3 xs)
                      backMatches = and $ zipWith ($) [isOne, isZero, isOne] (takeLast 3 xs)
                  in if frontMatches && backMatches
                    then Just $ take (length xs - 6) (drop 3 xs)
                    else Nothing

rle xs = map (\g -> if (head g) == '1' then  One (length g) else Zero (length g)) (List.group xs)

              
dropEnclosingWhiteSpace rle_row = take ((length rle_row)-2) (drop 1 rle_row)


takeRow pgm row_num = let rlen = fromIntegral row_num
                          imgw = fromIntegral $ PGM.width pgm
                       in  L.take imgw (L.drop (rlen * imgw) (PGM.bytes pgm))

threshold x
  | x > 200  = 255
  | otherwise = 0


toBinary b
  | b == 255 = '0'
  | otherwise = '1'

readApplyThreshAndWrite input_filename output_filename = 
  do
    pgmFileStream <- L.readFile input_filename
    maybePgm <- return $ PGM.parse pgmFileStream
    case maybePgm of 
      Nothing -> putStrLn "Failed to parse"
      Just (gr,m) -> do
                       let newData = L.map threshold (PGM.bytes gr)
                       print $ L.length newData
                       let newPGM = PGM.PGM{ PGM.pgmType = (PGM.pgmType gr),
                                             PGM.comments = (PGM.comments gr),
                                             PGM.width = (PGM.width gr),
                                             PGM.height = (PGM.height gr),
                                             PGM.maxPixel = (PGM.maxPixel gr),
                                             PGM.bytes = newData }
                       PGM.writeFile output_filename newPGM



One a |-| One b = abs $ a - b

Nothing >>? _ = Nothing
Just v  >>? f = f v

Nothing >>?! _ = return ()
Just v  >>?! f =  f v


isOne x = case x of 
            One _  -> True
            otherwise -> False
isZero x = case x of 
              Zero _  -> True
              otherwise -> False

takeLast n xs = drop (length xs - n) xs


lcodes = ["0001101", "0011001", "0010011", "0111101", "0100011", 
          "0110001", "0101111", "0111011", "0110111", "0001011"]

rcodes = inverse <$> lcodes
gcodes = reverse <$> rcodes

fcodes = ["LLLLLL", "LLGLGG", "LLGGLG", "LLGGGL", "LGLLGG", 
          "LGGLLG", "LGGGLL", "LGLGLG", "LGLGGL", "LGGLGL" ]

inverse xs = flipBit <$> xs
  where flipBit '0' = '1'
        flipBit _ = '0'
