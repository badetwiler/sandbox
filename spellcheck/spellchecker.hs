import System.IO
import Data.List.Split
import Control.Applicative
import Data.Char 

import qualified Data.Set as Set
import qualified Data.Map as Map
import qualified Data.List as List

type Hist = Map.Map String Int 

main = do 
        putStrLn "Weclome"
        putStrLn ""
        hist <- trainFromFile "big.txt"
        correctAndLoop hist
        putStrLn ""
        putStrLn "Done."


correctAndLoop :: Hist -> IO ()
correctAndLoop hist = do 
                        putStr "input: "
                        word <- getLine
                        putStrLn ""
                        case word of "exit" -> return ()
                                     word -> let correction = correct word hist
                                              in do putStrLn $ "did you mean " ++ correction ++ "?"
                                                    correctAndLoop hist
 


trainFromFile :: String -> IO Hist
trainFromFile fileName = do
                            words <- separateWords <$> readFile "big.txt"
                            return $ train words

train :: [String] -> Hist
train features = Map.fromList [ (x, count x features) | x <- features] 
                            

correct :: String -> Hist -> String 
correct word hist = 
    let candidates = [ known [word] hist ] ++ 
                     [ known (Set.toList $ edits1 word ) hist ] ++ 
                     [ known_edits2 word hist ]
        candidateList = case List.find (\x -> Set.size x > 0) candidates of Just s -> (Set.toList s)
                                                                            Nothing -> [""]
    in case ( maxValueForKey candidateList hist ) of Just x -> x
                                                     Nothing -> word

maxValueForKey :: [String] -> Hist -> Maybe String
maxValueForKey [] _ = Nothing
maxValueForKey keys hist = let tuples = map (\k -> (k, Map.lookup k hist) ) keys 
                               (mk,mv) = foldl (\(mk,mv) (k,v) -> if v > mv then (k,v) else (mk,mv)) ("",Nothing) tuples
                           in Just mk

known :: [String] -> Hist -> Set.Set String 
known words hist = Set.fromList [w | w <- words, Map.member w hist]

known_edits2 :: String -> Hist -> Set.Set String
known_edits2 word hist = Set.fromList [e2 | e1 <- Set.toList $ edits1 word, 
                                            e2 <- Set.toList $ edits1 e1, 
                                            Map.member e2 hist ]


count :: String -> [String] -> Int
count x xs = length $ filter (\x' -> x' == x) xs


edits1 :: String -> Set.Set String
edits1 word = 
    let splits = [ (take n word, drop n word) | n <- [0..length word] ]
        deletes = [ a ++ (drop 1 b)  | (a,b) <- splits, b /= "" ]
        transposes = [a ++ [b !! 1] ++ [head b] ++ (drop 2 b) | (a,b) <- splits, length b > 1]
        replaces = [a ++ [c] ++ (drop 1 b) | (a,b) <- splits, c <- ['a'..'z'], b /= "" ]
        inserts = [a ++ [c] ++ b | (a,b) <- splits, c <- ['a'..'z'] ]
    in Set.fromList (deletes ++ transposes ++ replaces ++ inserts)



separateWords::String -> [String]
separateWords words = (\word -> map toLower word) <$> filter isAllChar (splitOn " " words)


isAllChar :: [Char] -> Bool
isAllChar [] = True
isAllChar (x:xs) 
    | (ord x  >= a) && (ord x <= z) = isAllChar xs
    | (ord x  >= capA) && (ord x <= capZ) = isAllChar xs
    | otherwise = False 
    where a = ord 'a'
          z = ord 'z'
          capA = ord 'A'
          capZ = ord 'Z'


