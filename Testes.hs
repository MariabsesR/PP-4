module Testes (testar) where

import Data.List (nub)
import Data.Map (Map)
import Labirintos
  ( EstadoJogo (..),
    chaves,
    encontraPosicao,
    getSymbol,
    inicializa,
    jogador,
    move,
    posicaoFinal,
    substitui,
    temosChave,
    terminado,
    thisPortais,
  )
import System.Random
import Test.QuickCheck

testar :: IO ()
testar = do
  quickCheck prop_dimensoes
  quickCheck prop_limites_jogo
  quickCheck prop_chaves
  quickCheck prop_portas
  quickCheck prop_num_chaves
  quickCheck prop_portais
  quickCheck prop_using_doors
  quickCheck prop_using_portals
  quickCheck prop_terminou

{- Propriedade a ser testada :
    Dimensoes de um labirinto nao mudam apos efetuar uma sequencia de movimentos
-}
prop_dimensoes :: EstadoJogo -> Movimentos -> Bool
prop_dimensoes j (Movimentos m) = move j m `sameDim` j

sameDim :: EstadoJogo -> EstadoJogo -> Bool
(EstadoJogo newlab _ _ _ _ _) `sameDim` (EstadoJogo oldlab _ _ _ _ _) = altura && largura
  where
    altura = length newlab == length oldlab
    larguraOld = foldl (\acc x -> length x : acc) [] oldlab
    larguraNew = foldl (\acc x -> length x : acc) [] newlab
    largura = larguraOld == larguraNew

{- Propriedade a ser testada :
   Jogador nao sai dos limites do mapa apos efetuar uma sequencia de movimentos
-}
prop_limites_jogo :: EstadoJogo -> Movimentos -> Bool
prop_limites_jogo ej (Movimentos m) = jogadorNoInterior (move ej m)

jogadorNoInterior :: EstadoJogo -> Bool
jogadorNoInterior ej = x > 0 && y > 0 && y < altura -1 && x < largura -1
  where
    (x, y) = jogador ej
    labirinto = getView ej
    altura = length labirinto
    largura = length (head labirinto)

{- Propriedade a ser testada :
   numero de chaves na posse do jogador nao diminui apos efetuar uma sequencia de movimentos. -}
prop_chaves :: EstadoJogo -> Movimentos -> Bool
prop_chaves e (Movimentos m) = length (chaves e) <= length (chaves (move e m))

{- Propriedade a ser testada :
   numero de portas presentes num labirinto nao aumenta apos efetuar uma sequencia de movimentos.-}
prop_portas :: EstadoJogo -> Movimentos -> Bool
prop_portas e (Movimentos m) = numeroPortas (getView e) >= numeroPortas (getView (move e m))

{- getView retorna o labirinto numa lista onde se ve a posicao do jogador-}
getView :: EstadoJogo -> [String]
getView ej = take (length showJogo -1) showJogo
  where
    showJogo = lines (show ej)

numeroPortas :: [String] -> Int
numeroPortas [] = 0
numeroPortas (x : xs) = length (filter (`elem` portas) x) + numeroPortas xs
  where
    portas = ['A', 'B', 'C']

{- Propriedade a ser testada :
  Se apos os movimentos o jogador ocupar a posicao final entao o jogo esta terminado
-}
prop_terminou :: EstadoJogo -> Movimentos -> Property
prop_terminou e (Movimentos m) = jogador novoEstado == posicaoFinal e ==> terminado novoEstado
  where
    novoEstado = move e m

{- Propriedade a ser testada :
  Numero de chaves na posse do jogador aumenta só se o numero de chaves soltas no labirinto diminuir
-}

prop_num_chaves :: EstadoJogo -> Movimentos -> Bool
prop_num_chaves e (Movimentos m) = length (chaves novoEstado) == chavesGanhas + chavesPosseJogador
  where
    novoEstado = move e m
    chavesPosseJogador = length (chaves e)
    chavesGanhas = numChaves (getView e) - numChaves (getView novoEstado)

numChaves :: [String] -> Int
numChaves [] = 0
numChaves (x : xs) = length (filter (`elem` possiveisChaves) x) + numChaves xs
  where
    possiveisChaves = ['a', 'b', 'c']

{- Propriedade a ser testada :
  Verifica o numero de portais visiveis num labirinto apos efetuar uma sequencia de movimentos
  Sendo que podem ser 0, 2 ou 1. Caso seja 1 entao significa que o jogador
  esta a ocupar a posicao de um dos portais
-}

prop_portais :: EstadoJogo -> Movimentos -> Bool
prop_portais e (Movimentos m)
  | nPortaisAntesMov == 0 && nPortaisDepoisMov == 0 = True
  | nPortaisAntesMov == 2 = nPortaisDepoisMov == 1 && jogador ej `elem` thisPortais ej || nPortaisDepoisMov == 2
  | otherwise = False
  where
    nPortaisAntesMov = contarOcurrencias (getView e) '@'
    ej = move e m
    nPortaisDepoisMov = contarOcurrencias (getView ej) '@'

contarOcurrencias :: [String] -> Char -> Int
contarOcurrencias lab c = foldr ((+) . length . filter (`elem` (c : ""))) 0 lab

{- Propriedade a ser testada:
    Se a proxima posicao do jogador for uma porta e o jogador nao possuir a chave em quesao entao
    a posicao do jogador nao se altera -}
prop_using_doors :: EstadoJogo -> Movimentos -> Bool
prop_using_doors ej (Movimentos ms) = usingDoors ej ms

usingDoors :: EstadoJogo -> String -> Bool
usingDoors ej [] = True
usingDoors ej (m : ms)
  | x `elem` portas && not (temosChave ej x) = jogador (move ej [m]) == jogador ej
  | otherwise = usingDoors (move ej [m]) ms
  where
    portas = ['A', 'B', 'C']
    x = getSymbol ej (posicaoDestino ej m)

posicaoDestino :: EstadoJogo -> Char -> (Int, Int)
posicaoDestino (EstadoJogo lab c f (a, b) s p) m
  | m == 'u' = (a -1, b)
  | m == 'd' = (a + 1, b)
  | m == 'l' = (a, b -1)
  | otherwise = (a, b + 1)

{- Propriedade a ser testada:
    Se o jogador se mover para um portal entao a proxima posicao do jogador vai ser
    a posicao do outro portal -}

prop_using_portals :: EstadoJogo -> Movimentos -> Bool
prop_using_portals ej (Movimentos ms) = usingPortals ej ms

usingPortals :: EstadoJogo -> String -> Bool
usingPortals ej [] = True
usingPortals ej (m : ms)
  | proximaPos `elem` listaPortais = jogador (move ej [m]) == outroPortal
  | otherwise = usingPortals (move ej [m]) ms
  where
    listaPortais = thisPortais ej
    proximaPos = posicaoDestino ej m
    outroPortal = head (filter (/= proximaPos) (thisPortais ej))

{- Sao gerados labirintos quadrados de dimensao minima igual a 4 e maxima igual a 10
   Minima sendo 4 para que os labirintos validos ficando assim com 4 posicoes livres que nao
   sejam obrigatoriamente paredes
   maxima 10 porque gera uma amostra significativa de labirintos-}
instance Arbitrary EstadoJogo where
  arbitrary = do
    dim <- choose (4, 10) :: Gen Int
    interior <- makeInterior dim
    let interiorAndWalls = map addWall interior
    let wall = replicate dim '*'
    let labnoSF = [wall] ++ interiorAndWalls ++ [wall]
    yesPortal <- choose (False, True) :: Gen Bool
    pos <- gerarPos dim
    let posToSubstitute = getPos yesPortal pos
    let usePos = getPos yesPortal pos
    let result = insertKeyPositions labnoSF usePos
    let posJog = encontraPosicao 'S' result
    return $ inicializa result posJog []

insertKeyPositions :: [String] -> [(Int, Int)] -> [String]
insertKeyPositions labnoSF pos = if length pos == 2 then insertSF else insertPortals
  where
    insertSF = substitui (substitui labnoSF (head pos) 'S') (pos !! 1) 'F'
    insertPortals = substitui (substitui insertSF (pos !! 2) '@') (pos !! 3) '@'

getPos :: Bool -> [(Int, Int)] -> [(Int, Int)]
getPos b l = if b then take 4 (nub l) else take 2 (nub l)

gerarPos :: Int -> Gen [(Int, Int)]
gerarPos dim = vectorOf 100 (posGenerator dim) :: Gen [(Int, Int)]

posGenerator :: Int -> Gen (Int, Int)
posGenerator dim = do
  x <- choose (1, dim -2) :: Gen Int
  y <- choose (1, dim -2) :: Gen Int
  return (x, y)

makeInterior :: Int -> Gen [String]
makeInterior dim = do
  let line = vectorOf (dim -2) gerarSimbolos
  vectorOf (dim -2) line

addWall :: String -> String
addWall x = "*" ++ x ++ "*"

gerarSimbolos :: Gen Char
gerarSimbolos = elements ['*', ' ', 'a', 'b', 'c', 'A', 'B', 'C']

newtype Movimentos = Movimentos String deriving (Show)

{- sao gerados movimentos com pelo menos uma direcao -}
instance Arbitrary Movimentos where
  arbitrary = do
    s <- geraMovimentos
    return $ Movimentos s

geraMovimentos :: Gen String
geraMovimentos = listOf1 $ elements ['u', 'd', 'l', 'r']
