module PGM  
( PGM(..),
  parse,
  PGM.readFile,
  PGM.writeFile,
) where  

import System.IO
import Control.Applicative
import Data.Char 

import qualified Data.ByteString.Lazy.Char8 as L8
import qualified Data.ByteString.Lazy as L
import Data.Char (isSpace)

data PGM = PGM {
    pgmType :: String,
    comments :: [String],
    width :: Int,
    height :: Int,
    maxPixel :: Int,
    bytes :: L.ByteString } deriving (Eq)


instance Show PGM where
    show (PGM typ cmts w h m _) = "PGM { type:" ++ typ ++ " " ++
                                         "comments:" ++ show cmts ++ " " ++ 
                                         "size:" ++ show w ++ "x" ++ show h ++ " " ++
                                         "maxPixel:" ++ show m ++ " }"

writeFile :: String -> PGM -> IO ()
readFile :: String -> IO (Maybe PGM)
parse :: L.ByteString -> Maybe (PGM, L.ByteString)
matchHeader :: L.ByteString -> L.ByteString -> Maybe L.ByteString
getBytes :: Int -> L.ByteString -> Maybe (L.ByteString, L.ByteString)
matchInt :: L.ByteString ->  Maybe (Int, L.ByteString)
(>>?) :: Maybe a -> (a -> Maybe b) -> Maybe b
(>>?!) :: Maybe a -> (a -> IO ()) -> IO ()


main = do 
        putStrLn "Weclome"
        putStrLn ""
        pgmFileStream <- L.readFile "barcode1.pgm"
        pgm <- return $ parse pgmFileStream
        print pgm


writeFile fileName pgm = do 
                           let header = (pgmType pgm) ++ "\n" ++
                                        (concatMap (\c -> "#" ++ c ++ "\n") (comments pgm)) ++
                                        (show $ width pgm)  ++ " " ++ (show $ height pgm) ++ "\n" ++
                                        (show $ maxPixel pgm) ++ "\n"
                             in L8.writeFile fileName $ L.append (L8.pack header) (bytes pgm)

readFile fileName = do 
                     contents <- L.readFile fileName
                     case parse contents of 
                        Nothing -> return Nothing
                        Just (pgm,rest) -> return $ Just pgm


parse s = 
    matchHeader (L8.pack "P5") s                                                >>?  
    \rest -> Just( L8.dropWhile isSpace rest )                                  >>?
    \rest -> Just ( matchComments rest )                                        >>?
    \(comments,rest) -> matchInt (L8.dropWhile isSpace rest)                    >>?
    \(width,rest) -> matchInt  (L8.dropWhile isSpace rest)                      >>?  
    \(height,rest) -> matchInt (L8.dropWhile isSpace rest)                      >>?  
    \(maxGrayVal,rest) -> getBytes (width*height) (L8.dropWhile isSpace rest)   >>? 
    \(bytes, rest) -> Just ( PGM "P5" (L8.unpack <$> comments) width height maxGrayVal bytes , rest) 
            

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


matchComments :: L.ByteString -> ( [L.ByteString], L.ByteString )
matchComments str = matchComments' [] str
  where matchComments' comments str = 
          if (L8.pack "#") `L.isPrefixOf` str
          then matchComments' newComments newString
          else  (comments, str)
            where len = ( L.length $ L8.takeWhile (/='\n') str  ) - 1
                  newComments = comments ++ [ L8.take len (L8.drop 1 str) ]
                  newString = L8.drop (len + 2) str


matchHeader prefix str 
  | prefix `L.isPrefixOf` str = Just ( L.drop (L.length prefix) str )
  | otherwise = Nothing 

Nothing >>? _ = Nothing
Just v  >>? f = f v

Nothing >>?! _ = return ()
Just v  >>?! f =  f v

