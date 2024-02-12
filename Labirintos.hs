module Labirintos
  ( EstadoJogo (..),
    inicializa,
    jogador,
    terminado,
    chaves,
    posicaoFinal,
    move,
    inicializeLab,
    getInstruction,
    makeSaveLab,
    makeMoveLab,
    getLabFilePath,
    encontraPosicao,
    substitui,
    thisPortais,
    findOtherPortal,
    getSymbol,
    temosChave,
    labir,
  )
where

import Data.Char (toLower)
import Data.List (elemIndices, findIndices, sort)

inicializeLab :: String -> EstadoJogo
inicializeLab map = inicializa (drop 2 (lines map)) (read (head (lines map)) :: (Int, Int)) (head (drop 1 (lines map)))

getInstruction :: String -> String
getInstruction instrucao = head (words instrucao)

makeSaveLab :: EstadoJogo -> String
makeSaveLab lab = show (jogador lab) ++ "\n" ++ chaves lab ++ "\n" ++ labir lab

makeMoveLab :: EstadoJogo -> String -> EstadoJogo
makeMoveLab lab instruction = move lab $ unwords (tail (words instruction))

getLabFilePath :: String -> String
getLabFilePath instruction = unwords $ tail (words instruction)

data EstadoJogo = EstadoJogo
  { labirinto :: [String],
    chave :: [Char],
    posFinal :: (Int, Int),
    posJog :: (Int, Int),
    posIn :: (Int, Int),
    portais :: [(Int, Int)]
  }

inicializa :: [String] -> (Int, Int) -> [Char] -> EstadoJogo
inicializa labirinto posInicJog chaves = EstadoJogo labirinto chaves (encontraPosicao 'F' labirinto) posInicJog posInic (portalList labirinto)
  where
    posInic = encontraPosicao 'S' labirinto

encontraPosicao :: Char -> [String] -> (Int, Int)
encontraPosicao c xs = (i, j)
  where
    i = length $ takeWhile (notElem c) xs
    j = length $ takeWhile (/= c) (xs !! i)

labir :: EstadoJogo -> String
labir (EstadoJogo lab _ _ _ _ _) = unlines lab

jogador :: EstadoJogo -> (Int, Int)
jogador (EstadoJogo _ _ _ posJog _ _) = posJog

chaves :: EstadoJogo -> String
chaves (EstadoJogo _ chave _ _ _ _) = chave

thisPortais :: EstadoJogo -> [(Int, Int)]
thisPortais (EstadoJogo _ _ _ _ _ portais) = portais

posicaoFinal :: EstadoJogo -> (Int, Int)
posicaoFinal (EstadoJogo _ _ posFinal _ _ _) = posFinal

terminado :: EstadoJogo -> Bool
terminado (EstadoJogo _ _ posFin posJog _ _) = posFin == posJog

instance Show EstadoJogo where
  show (EstadoJogo lab chave posFinal posJog posIn portais) = (foldl (\x acc -> x ++ acc ++ "\n") [] newLab) ++ "chaves: " ++ sort chave
    where
      newLab = substitui lab posJog 'P'

{- se o movimento nao for dulr entao retorna o mesmo estadoJogo-}
move :: EstadoJogo -> String -> EstadoJogo
move ej [] = ej
move ej (m : ms)
  | m == 'd' = move (executeMove ej (a + 1, b)) ms
  | m == 'u' = move (executeMove ej (a - 1, b)) ms
  | m == 'l' = move (executeMove ej (a, b - 1)) ms
  | m == 'r' = move (executeMove ej (a, b + 1)) ms
  | otherwise = move ej ms
  where
    (a, b) = jogador ej

executeMove :: EstadoJogo -> (Int, Int) -> EstadoJogo
executeMove ej pos
  | c == ' ' || c == 'S' || c == 'F' = moverJogador ej pos
  | c == 'a' || c == 'b' || c == 'c' = apagarChave ej pos c
  | c == 'A' || c == 'B' || c == 'C' = verificarPorta ej c pos
  | c == '@' = portalHandler ej pos
  | otherwise = ej
  where
    c = getSymbol ej pos

temosChave :: EstadoJogo -> Char -> Bool
temosChave ej porta = toLower porta `elem` chaves ej

verificarPorta :: EstadoJogo -> Char -> (Int, Int) -> EstadoJogo
verificarPorta ej c x = if temosChave ej c then moverJogador (apagarPorta ej x) x else ej

apagarPorta :: EstadoJogo -> (Int, Int) -> EstadoJogo
apagarPorta (EstadoJogo xs c f j s p) posicaoPorta = EstadoJogo novoLab c f posicaoPorta s p
  where
    novoLab = substitui xs posicaoPorta ' '

apagarChave :: EstadoJogo -> (Int, Int) -> Char -> EstadoJogo
apagarChave (EstadoJogo xs c f _ s p) posicaoChave = adicionarChave (EstadoJogo novoLab c f posicaoChave s p)
  where
    novoLab = substitui xs posicaoChave ' '

adicionarChave :: EstadoJogo -> Char -> EstadoJogo
adicionarChave (EstadoJogo lab listaChaves f j s p) c = EstadoJogo lab (c : listaChaves) f j s p

moverJogador :: EstadoJogo -> (Int, Int) -> EstadoJogo
moverJogador (EstadoJogo xs chave posFinal posJog posIn portais) novaPos = EstadoJogo xs chave posFinal novaPos posIn portais

getSymbol :: EstadoJogo -> (Int, Int) -> Char
getSymbol (EstadoJogo xs _ _ _ _ _) (i, j) = (xs !! i) !! j

portalHandler :: EstadoJogo -> (Int, Int) -> EstadoJogo
portalHandler (EstadoJogo xs chave posFinal posJog posIn portais) p = EstadoJogo xs chave posFinal newPlayerPos posIn portais
  where
    newPlayerPos = findOtherPortal portais p

substitui :: [String] -> (Int, Int) -> Char -> [String]
substitui xs p newSymbol = newLab
  where
    (beginning, toChange) = splitAt (fst p) xs
    line = head toChange
    (inicioLinha, fimLinha) = splitAt (snd p) line
    newLine = inicioLinha ++ [newSymbol] ++ tail fimLinha
    newLab = beginning ++ [newLine] ++ tail toChange

portalList :: [String] -> [(Int, Int)]
portalList xs = if length portalLines < 2 then sameLinePortal else portalPos
  where
    portalLines = findIndices (elem '@') xs
    portalCol = map ('@' `elemIndices`) (filter (elem '@') xs)
    unionpcol = concat portalCol
    portalPos = zip portalLines unionpcol
    sameLinePortal = zip (replicate 2 (head portalLines)) unionpcol

findOtherPortal :: [(Int, Int)] -> (Int, Int) -> (Int, Int)
findOtherPortal portais p = head (filter (/= p) portais)