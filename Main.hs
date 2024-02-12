import Data.Char (toLower)
import Data.List (delete, elemIndices, findIndices, sort)
import Labirintos (EstadoJogo, chaves, getInstruction, getLabFilePath, inicializa, inicializeLab, jogador, labir, makeMoveLab, makeSaveLab, move)
import System.Directory (doesFileExist)
import System.Directory.Internal.Prelude (getArgs)
import Testes (testar)

main :: IO ()
main = do
  args <- getArgs
  dispatch args

dispatch :: [String] -> IO ()
dispatch [] = do
  map <- readFile "default.map"
  game (inicializeLab map)
dispatch ["-t"] = testar
dispatch [ficheiro] = do
  fileExists <- doesFileExist ficheiro
  if fileExists
    then do
      map <- readFile ficheiro
      game (inicializeLab map)
    else usage
dispatch (_ : _) = usage

usage :: IO ()
usage = putStrLn "Utilização:\n ./Main [ficheiro] \n ./Main \n ./Main -t"

game :: EstadoJogo -> IO ()
game lab = do
  putStrLn (show lab)
  instrucao <- getLine
  makeInstruction lab instrucao

makeInstruction :: EstadoJogo -> String -> IO ()
makeInstruction lab instruction
  | getInstruction instruction == "move" = game (makeMoveLab lab instruction)
  | getInstruction instruction == "load" = loadNewLabirint (getLabFilePath instruction)
  | getInstruction instruction == "save" = saveLabirint (getLabFilePath instruction) lab
  | otherwise = return ()

loadNewLabirint :: FilePath -> IO ()
loadNewLabirint filepath = do
  fileExists <- doesFileExist filepath
  if fileExists
    then do
      map <- readFile filepath
      game $ inicializeLab map
    else return ()

saveLabirint :: FilePath -> EstadoJogo -> IO ()
saveLabirint filepath lab = do
  writeFile filepath $ makeSaveLab lab
  game lab
