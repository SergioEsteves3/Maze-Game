module Main where
import System.Environment ( getArgs )
import Data.List (intercalate)
import System.Directory (doesFileExist)
import Testes (test)
import Labirintos ( EstadoJogo(..), inicializa, move )

main :: IO ()
main = do
         arg <- getArgs
         case length arg of
            0 -> handleFile "default.map"
            1 -> handleArg (head arg)
            _ -> description

handleArg :: [Char] -> IO ()
handleArg arg = do
                  case arg of
                     "-t" -> test
                     _ -> checkFile arg

checkFile :: FilePath -> IO ()
checkFile file = do
                   exists <- doesFileExist file
                   if exists
                   then do handleFile file
                   else do description

handleFile :: FilePath -> IO ()
handleFile file = do
                    map <- readFile file
                    let lab = inicializa (lines map)
                    playerInput lab

description :: IO ()
description = do
                putStrLn "Uso:"
                putStrLn "./Main [ficheiro]     --carrega um ficheiro para jogar"
                putStrLn "./Main                --carrega o ficheiro default.map"
                putStrLn "./Main -t             --corre os testes"

playerInput :: EstadoJogo -> IO ()
playerInput lab = do
                    print lab
                    input <- getLine
                    let instructions = words input
                    case head instructions of
                            "move" -> moveIO lab (last instructions)
                            "load" -> load (last instructions)
                            "save" -> save lab (last instructions)
                            "exit" -> return ()
                            _ -> inputDescription lab --Instrucao invalida

moveIO :: EstadoJogo -> String -> IO ()
moveIO lab instruction = do
                            let newLab = move lab instruction
                            playerInput newLab

load :: String -> IO ()
load instruction = do
                     map <- readFile instruction
                     let lab = inicializa (lines map)
                     playerInput lab

save :: EstadoJogo -> FilePath -> IO ()
save lab filename = do
                      writeFile filename (unlines (show (jogador lab):(chaves lab:labirinto lab)))
                      playerInput lab

inputDescription :: EstadoJogo -> IO ()
inputDescription lab = do
                     putStrLn "Comandos:"
                     putStrLn "move                --movimenta o jogador"
                     putStrLn "save                --guarda o estado de jogo atual"
                     putStrLn "load                --carrega um estado de jogo"
                     putStrLn "exit                --termina a execucao da aplicacao"
                     playerInput lab