module Tarefa1 where

import Types
import System.Random

-- * Um Maze
-- | Exemplo
sampleMaze :: Maze
sampleMaze =
  [ [Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall],
    [Empty, Food Little, Food Little, Food Big, Food Little, Food Big, Food Little, Empty],
    [Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall]
  ]

-- * Função principal, junção de todas as auxiliares para gerar um Maze
-- | Nota: Se o comprimento for menor que 15 ou a altura menor que 10 devolve um erro e não gera um Maze
generateMaze :: Int -- ^ comprimento dos corredores do maze
                -> Int -- ^ altura do maze
                -> Int -- ^ seed que gera um maze aleatório
                -> Maze -- ^ maze resultante
generateMaze x y s
  | x < 15 || y < 10 = error "Maze inválido"
  | otherwise =
    let random_nrs = generateRandom ((x -2) * (y -2)) s
     in buildHouse $ openTunels $ addWalls (convertMaze $ subList (x -2) random_nrs) x

-- Funções relativas a gerar um Maze aleatório

-- | Dada uma seed devolve a lista de n inteiros aleatoriamente gerados
generateRandom :: Int -> Int -> [Int]
generateRandom n seed =
  let gen = mkStdGen seed -- cria um gerador aleatório
   in take n $ randomRs (0, 99) gen -- pega nos primeiros n elementos de uma série infinita de numeros aleatórios entre 0 e 9
                                      
-- | Dada uma seed devolve um inteiro aleatóriamente gerado
randomNrs :: Int -> Int
randomNrs seed = head $ generateRandom 1 seed

-- | Converte uma lista numa lista de listas de tamanho n
subList :: Int -> [a] -> [[a]]
subList _ [] = []
subList n l = take n l : subList n (drop n l)

-- | Converte uma lista de listas de inteiros num Maze
convertMaze :: [[Int]] -> Maze
convertMaze  = map convertCorridor 

-- | Converte uma lista de inteiros num Corridor usando a função convertPiece
convertCorridor :: [Int] -> Corridor
convertCorridor = map convertPiece 

-- | Converte um inteiro numa Piece

-- | n == 3 <=> Food Big

-- | 0 <= n < 70 <=> Food Little

-- | 70 < n <= 99 <=> Wall
convertPiece :: Int -> Piece
convertPiece x
  | x == 3 = Food Big
  | x >= 0 && x < 70 = Food Little
  | x >= 70 && x <= 99 = Wall

-- * Funções relativas a adicionar as paredes do maze

-- | Adiciona as paredes ao Maze
addWalls :: Maze -> Int -> Maze
addWalls l c = replicate c Wall : map verticalWalls l ++ [replicate c Wall]

-- | Adiciona paredes aos limites do(s) corredor(es) 
verticalWalls :: Corridor -> Corridor
verticalWalls c = Wall : c ++ [Wall]

-- * Função relativas a adiconar os túneis

-- | Substituiu os limites do(s) corredor(es) do meio do maze por Emptys (túneis)
openTunels :: Maze -> Maze
openTunels l
  | even y = take n l ++ [Empty : tail (init (l !! n)) ++ [Empty]] ++ [Empty : tail (init (l !! (n + 1))) ++ [Empty]] ++ drop (n + 2) l
  | otherwise = take m l ++ [Empty : tail (init (l !! n)) ++ [Empty]] ++ drop (m + 1) l
  where
    y = length l -- comprimento do maze
    m = div (y -1) 2 -- indíce do corredor do meio se a altura do maze for ímpar
    n = div y 2 - 1 --  indice de um dos corredores do meio se a altura do maze for par

-- * Funções relativas à adição da casa dos fantasmas no maze

-- | Substitui as peças do meio do labirinto pela casa dos fantasmas conforme a altura do maze seja par ou ímpar
buildHouse :: Maze -> Maze
buildHouse l
  | even c =
    take (ma -2) l
      ++ [takeBegCorr (mcp -1) (ma -2) l ++ replicate 10 Empty ++ takeEndCorr (mcp + 9) (ma -2) l]
      ++ [takeBegCorr (mcp -1) (ma -1) l ++ [Empty] ++ replicate 3 Wall ++ replicate 2 Empty ++ replicate 3 Wall ++ [Empty] ++ takeEndCorr (mcp + 9) (ma -1) l]
      ++ [takeBegCorr (mcp -1) ma l ++ [Empty] ++ Wall : replicate 6 Empty ++ [Wall] ++ [Empty] ++ takeEndCorr (mcp + 9) ma l]
      ++ [takeBegCorr (mcp -1) (ma + 1) l ++ [Empty] ++ replicate 8 Wall ++ [Empty] ++ takeEndCorr (mcp + 9) (ma + 1) l]
      ++ [takeBegCorr (mcp -1) (ma + 2) l ++ replicate 10 Empty ++ takeEndCorr (mcp + 9) (ma + 2) l]
      ++ drop (ma + 3) l
  | otherwise =
    take (ma -2) l
      ++ [takeBegCorr (mci -1) (ma -2) l ++ replicate 11 Empty ++ takeEndCorr (mci + 10) (ma -2) l]
      ++ [takeBegCorr (mci -1) (ma -1) l ++ [Empty] ++ replicate 3 Wall ++ replicate 3 Empty ++ replicate 3 Wall ++ [Empty] ++ takeEndCorr (mci + 10) (ma -1) l]
      ++ [takeBegCorr (mci -1) ma l ++ [Empty] ++ Wall : replicate 7 Empty ++ [Wall] ++ [Empty] ++ takeEndCorr (mci + 10) ma l]
      ++ [takeBegCorr (mci -1) (ma + 1) l ++ [Empty] ++ replicate 9 Wall ++ [Empty] ++ takeEndCorr (mci + 10) (ma + 1) l]
      ++ [takeBegCorr (mci -1) (ma + 2) l ++ replicate 11 Empty ++ takeEndCorr (mci + 10) (ma + 2) l]
      ++ drop (ma + 3) l
  where
    c = length $ head l --comprimento dos corredores
    ma = if even ll then ma1 - 1 else ma1 --indíce do corredor do meio dependendo se for par ou impar a altura do maze
    ma1 = div ll 2 --indíce do corredor do meio do maze
    ll = length l --altura do maze
    mcp = div (c - 8) 2  --metade do comprimento do corredor se c for par
    mci = div (c - 9) 2  --metade do comprimento do corredot se c for ímpar

-- | Põe aparte o início do corredor
takeBegCorr :: Int -- ^ número de peças do inicio do corredor a separar
               -> Int -- ^ índice do corredor
               -> Maze -- ^ maze onde se encontra o corredor
               -> Corridor -- ^ parte do corredor inicial onde se encontra a casa e separado desta
takeBegCorr mc ma l = take mc (l !! ma)

-- | Põe aparte o fim do corredor
takeEndCorr :: Int -- ^ indíce de onde acaba a casa e começa a parte final do corredor
               -> Int -- ^ indíce do corredor no maze
               -> Maze -- ^ maze onde se encontra o corredor
               -> Corridor -- ^ parte final do corredor separado da casa
takeEndCorr mc ma l = drop mc (l !! ma)
