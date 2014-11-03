module EAN13
( EAN13,
  encode,
  main
) where

import Control.Applicative
import Data.Char 


data EAN13 = EAN13 {
    firstGroup :: [Int],
    lastGroup :: [Int],
    checksum :: Int } deriving (Eq,Show)

main = do 
         let digits = [8,7,1,1,2,5,3,0,0,1,2,0]
         let checksum = encode digits
         print checksum




encode  digits 
  | length digits /= 12 = Nothing
  | otherwise = 
    let checksum = 10 - ( (sum $ zipWith ($) (cycle [id, (*3)] ) digits ) `mod` 10 )
        firstEncoding = ["LLLLLL", "LLGLGG", "LLGGLG", "LLGGGL", "LGLLGG", 
                         "LGGLLG", "LGGGLL", "LGLGLG", "LGLGGL", "LGGLGL" ]
        encoding = ( firstEncoding !! (head digits ) ) ++ "RRRRRR"
    in Just ( foldl (\acc (code,val) -> acc ++ (mapCode code val) ) 
                    "" 
                    (zip encoding ((tail digits)++[checksum])) )


mapCode char n 
  | char == 'L' = lcodes !! n
  | char == 'G' = gcodes !! n
  | otherwise   = rcodes !! n
  where lcodes = ["0001101", "0011001", "0010011", "0111101", "0100011", 
                  "0110001", "0101111", "0111011", "0110111", "0001011"]
        rcodes = inverse <$> lcodes
        gcodes = reverse <$> rcodes

inverse xs = flipBit <$> xs
  where flipBit '0' = '1'
        flipBit _ = '0'







