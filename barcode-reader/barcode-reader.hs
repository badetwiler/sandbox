import System.IO
import Data.List.Split
import Control.Applicative
import Data.Char 

import qualified PGM
import qualified Data.Set as Set
import qualified Data.Map as Map
import qualified Data.ByteString.Lazy as L
import Data.Char (isSpace)

main = do 
        putStrLn "Weclome"
        putStrLn "" 
        pgmFileStream <- L.readFile "barcode1.pgm"
        maybePgm <- return $ PGM.parsePGM pgmFileStream
        case maybePgm of 
          Nothing -> putStrLn "Failed to parse"
          Just (gr,m) -> print gr











