module Labirintos (
    EstadoJogo,
    labirinto,
    inicializa,
    jogador,
    chaves,
    terminado,
    move,
    size,
    portais
) where
import Data.List ( insert )
import Data.Char ( toLower )
import LabString ( posicao, getSymbol, replaceSymbol, findPortals )

data EstadoJogo = EstadoJogo { labirinto :: [String]
                             , jogador :: (Int, Int)
                             , fim :: (Int,Int)
                             , chaves :: String
                             , portais :: [(Int,Int)]
                             }

inicializa :: [String] -> EstadoJogo
inicializa lab = EstadoJogo {labirinto = drop 2 lab,
                             jogador = read (head lab),
                             fim = posicao 'F' lab,
                             chaves = lab !! 1,
                             portais = map (\(i, Just j) -> (i,j)) (findPortals (drop 2 lab)) --remover Maybe quando ha portal
                             }

terminado :: EstadoJogo -> Bool
terminado ej = jogador ej == fim ej

size :: EstadoJogo -> (Int, Int)
size ej = (length (labirinto ej), length (head (labirinto ej)))

instance Show EstadoJogo where
         show (EstadoJogo lab jogador fim chaves portais) = unlines (replaceSymbol jogador lab 'P') ++ ("chaves: "++ chaves)

move :: EstadoJogo -> String -> EstadoJogo
move ej [] =  ej
move ej (x:xs) | x == 'r' = move (movePlayer (jogador ej) (0,1) ej) xs
               | x == 'l' = move (movePlayer (jogador ej) (0,-1) ej) xs
               | x == 'u' = move (movePlayer (jogador ej) (-1,0) ej) xs
               | x == 'd' = move (movePlayer (jogador ej) (1,0) ej) xs
               | otherwise = move ej xs

movePlayer :: (Int, Int) -> (Int, Int) -> EstadoJogo -> EstadoJogo
movePlayer (w,z) (x,y) ej    | (w + x < 0) || (z + y < 0) = ej
                             | getSymbol (labirinto ej) (w + x, z + y) `elem` " SF" = EstadoJogo (labirinto ej) (w + x, z + y) (fim ej) (chaves ej) (portais ej)
                             | getSymbol (labirinto ej) (w + x, z + y) == '*' = ej
                             | getSymbol (labirinto ej) (w + x, z + y) `elem` "abc" = handleKeys ej (w + x, z + y)
                             | getSymbol (labirinto ej) (w + x, z + y) `elem` "ABC" = handleDoors ej (w + x, z + y)
                             | otherwise = handlePortals ej (w + x, z + y)

handleKeys :: EstadoJogo -> (Int,Int) -> EstadoJogo
handleKeys ej pos = EstadoJogo (replaceSymbol pos (labirinto ej) ' ') pos (fim ej) (insert (getSymbol (labirinto ej) pos) (chaves ej)) (portais ej)

handleDoors :: EstadoJogo -> (Int, Int) -> EstadoJogo
handleDoors ej pos | toLower (getSymbol (labirinto ej) pos) `elem` chaves ej = EstadoJogo (replaceSymbol pos (labirinto ej) ' ') pos (fim ej) (chaves ej) (portais ej)
                     | otherwise = ej

handlePortals :: EstadoJogo -> (Int, Int) -> EstadoJogo
handlePortals ej pos   | pos == head (portais ej) = EstadoJogo (labirinto ej) (last (portais ej)) (fim ej) (chaves ej) (portais ej)
                       | otherwise = EstadoJogo (labirinto ej) (head (portais ej)) (fim ej) (chaves ej) (portais ej)