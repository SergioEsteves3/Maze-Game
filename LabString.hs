module LabString (posicao, findPortals, getSymbol, replaceSymbol) where
import Data.List (elemIndex, findIndices)

posicao :: Char -> [String] -> (Int, Int)
posicao c xs = posicao xs 0
            where posicao [] _ = (-1,-1) -- nunca acontece (assumindo labirnitos validos)  
                  posicao (x:xs) col | line /= -1 = (col,line)
                                     | otherwise = posicao xs (col + 1)
                                       where line = findSymbol x c

findSymbol :: String -> Char -> Int
findSymbol xs x | index == length xs = -1
                | otherwise = index
                  where index = length (takeWhile (/= x) xs)

getSymbol :: [String] -> (Int, Int) -> Char
getSymbol lab (x,y) = lab !! x !! y

replaceSymbol :: (Int, Int) -> [String] -> Char -> [String]
replaceSymbol (x,y) lab c = fst splitLab ++ ( (fst splitLine ++ (c : tail (snd splitLine))) : tail (snd splitLab))
                            where splitLab = splitAt x lab
                                  splitLine = splitAt y (head (snd splitLab))

findPortals:: [String] -> [(Int,Maybe Int)]
findPortals lab | null columns = []
                | otherwise =[(head columns, elemIndex '@' (lab !! head columns)), (last columns, elemIndex '@' (lab !! last columns))]
                  where columns = findIndices (elem '@') lab