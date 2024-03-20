module Testes (test) where
import Test.QuickCheck
import Labirintos (EstadoJogo (..), move, size, chaves, portais, inicializa)
import LabString (replaceSymbol, posicao)
test :: IO ()
test = do
        quickCheck prop_move_length
        quickCheck prop_move_player_in_bounds
        quickCheck prop_move_key_size
        quickCheck prop_move_door_nr
        quickCheck prop_move_finish_position
        quickCheck prop_move_portal_location
        quickCheck prop_move_start_position
        quickCheck prop_move_key_map_amount


--Verifica que as dimensões do mapa se mantem constantes apos o movimento
prop_move_length :: EstadoJogo -> Movimentos -> Bool
prop_move_length ej mov= size ej == size (move ej (moves mov))

--Verifica que o jogador continua dentro do mapa apos o movimento
prop_move_player_in_bounds :: EstadoJogo -> Movimentos -> Bool
prop_move_player_in_bounds ej mov = (fst (jogador (move ej (moves mov))) < fst (size ej) && fst (jogador (move ej (moves mov))) > 0)
                                     && (snd (jogador (move ej (moves mov))) < snd (size ej) && snd (jogador (move ej (moves mov))) > 0)

--Verifica que a quantidade de chaves na posse do jogador nao diminui apos o movimento
prop_move_key_size :: EstadoJogo -> Movimentos -> Bool
prop_move_key_size ej mov = length (chaves (move ej (moves mov))) >= length (chaves ej)

--Verifica que a quantidade de portas no mapa nao aumenta apos o movimento
prop_move_door_nr :: EstadoJogo -> Movimentos -> Bool
prop_move_door_nr ej mov = length (filter (`elem` "ABC") ( unlines (labirinto  ej))) >=
                           length (filter (`elem` "ABC") ( unlines (labirinto (move ej (moves mov)))))

--Verifica que a posicao final no mapa nao se altera apos o movimento
prop_move_finish_position :: EstadoJogo -> Movimentos -> Bool
prop_move_finish_position ej mov = posicao 'F' (labirinto ej) == posicao 'F' (labirinto (move ej (moves mov)))

--Verifica que a localizacao dos dois portais continua a mesma apos o movimento
prop_move_portal_location :: EstadoJogo -> Movimentos -> Bool
prop_move_portal_location ej mov = portais ej == portais (move ej (moves mov))

--Verifica que a posicao inicial do jogo nao se altera apos o movimento
prop_move_start_position :: EstadoJogo -> Movimentos -> Bool
prop_move_start_position ej mov = posicao 'S' (labirinto ej) == posicao 'S' (labirinto (move ej (moves mov)))

--Verifica que a quantidade de chaves no mapa nao aumenta apos  o movimento
prop_move_key_map_amount :: EstadoJogo -> Movimentos -> Bool
prop_move_key_map_amount ej mov = length (filter (`elem` "abc") ( unlines (labirinto  ej))) >=
                                  length (filter (`elem` "abc") ( unlines (labirinto (move ej (moves mov)))))

newtype Movimentos = Movimentos { moves :: String} deriving (Show)

instance Arbitrary Movimentos where
    arbitrary = do
                  n <- choose (0,100) :: Gen Int --tamanho maximo grande o suficiente para muitas variacoes
                  Movimentos <$> movGen n ""

instance Arbitrary EstadoJogo where
    arbitrary = do
                  height <- choose (5,30) :: Gen Int --tamanho maximo grande o suficiente para muitas variacoes
                  width  <- choose (5,30) :: Gen Int --tamanho maximo grande o suficiente para muitas variacoes
                  start <- choose ((1,1), (height - 2, width - 2)) :: Gen (Int,Int)
                  finish <- choose ((1,1), (height - 2, width - 2)) :: Gen (Int,Int)
                  lab <- labGen (height - 2) (width - 2) []
                  let labS = replaceSymbol start lab 'S'
                  let labF = replaceSymbol finish labS 'F'
                  oneof [return (inicializa ([show start, ""] ++ labF)), returnPortalMap labF start height width]

returnPortalMap :: [String] -> (Int, Int) -> Int -> Int -> Gen EstadoJogo
returnPortalMap lab start height width = do
                            portal1 <- choose ((1,1), (height - 2, width - 2)) :: Gen (Int,Int)
                            portal2 <- choose ((1,1), (height - 2, width - 2)) :: Gen (Int,Int)
                            let labp1 = replaceSymbol portal1 lab '@'
                            let labp2 = replaceSymbol portal2 labp1 '@'
                            return (inicializa ([show start, ""] ++ labp2))

lineGen :: Int -> String -> Gen String
lineGen 0 acc = return acc
lineGen width acc = do
                symbol <- frequency [(50, elements " *"), (1, elements "abc"), (1, elements "ABC")] --muitos mais espacos e paredes que portas/chaves
                lineGen (width-1) (symbol:acc)

labGen :: Int -> Int -> [String] -> Gen [String]
labGen 0 width acc = return (wall : (acc ++ [wall]))
                     where wall = replicate (width + 2) '*'
labGen height width acc = do
                 innerLine <- lineGen width ""
                 let line = ('*':innerLine) ++ "*"
                 labGen (height - 1) width (line:acc)

movGen :: Int -> String -> Gen String
movGen 0  acc = return acc
movGen n acc = do
                 dir <- elements "rlud"
                 movGen (n-1) (dir:acc)
