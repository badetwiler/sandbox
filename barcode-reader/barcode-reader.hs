import System.IO
import Data.List.Split
import Control.Applicative
import Data.Char 

import qualified Data.Set as Set
import qualified Data.Map as Map
import qualified Data.List as List
import qualified Data.ByteString.Lazy.Char8 as L8
import qualified Data.ByteString.Lazy as L
import Data.Char (isSpace)

data Greymap = Greymap {
    greyWidth :: Int,
    greyHeight :: Int,
    greyMax :: Int,
    greyData :: L.ByteString } deriving (Eq)

type Hist = Map.Map String Int 

instance Show Greymap where
    show (Greymap w h m _) = "Greymap " ++ show w ++ "x" ++ show h ++
                             " " ++ show m

parsePGM :: L.ByteString -> Maybe (Greymap, L.ByteString)
matchHeader :: L.ByteString -> L.ByteString -> Maybe L.ByteString
getBytes :: Int -> L.ByteString -> Maybe (L.ByteString, L.ByteString)
matchInt :: L.ByteString ->  Maybe (Int, L.ByteString)
(>>?) :: Maybe a -> (a -> Maybe b) -> Maybe b
(>>?!) :: Maybe a -> (a -> IO ()) -> IO ()


main = do 
        putStrLn "Weclome"
        putStrLn ""
        pgmFileStream <- L.readFile "barcode1.pgm"
        pgm <- return $ parsePGM pgmFileStream
        print pgm



parsePGM s = 
    matchHeader (L8.pack "P5") s                                                >>?  
    \rest -> Just( L8.dropWhile isSpace rest )                                  >>?
    \rest -> let len = (+1) (L.length (L8.takeWhile (/='\n') rest)) 
             in Just( L8.drop len rest )                                        >>?
    \rest -> matchInt (L8.dropWhile isSpace rest)                               >>?
    \(width,rest) -> matchInt  (L8.dropWhile isSpace rest)                      >>?  
    \(height,rest) -> matchInt (L8.dropWhile isSpace rest)                      >>?  
    \(maxGrayVal,rest) -> getBytes (width*height) (L8.dropWhile isSpace rest)   >>? 
    \(bytes, rest) -> Just ( Greymap width height maxGrayVal bytes , rest) 
            

getBytes n str = let len = fromIntegral n 
                     takeResult = L.take len str
                 in if L.length takeResult == len
                    then Just (takeResult, L.drop len str)
                    else Nothing
                     

matchInt s = case L8.readInt s of
               Nothing -> Nothing
               Just (num,rest) 
                 | num <= 0 -> Nothing
                 | otherwise -> Just (num,rest)


matchHeader prefix str 
  | prefix `L.isPrefixOf` str = Just ( L.drop (L.length prefix) str )
  | otherwise = Nothing 


Nothing >>? _ = Nothing
Just v  >>? f = f v

Nothing >>?! _ = return ()
Just v  >>?! f =  f v

