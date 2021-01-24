
{-# LANGUAGE LambdaCase #-}
module Tarefa3 where

import Types

{- * Estas funções compactam um maze numa 
lista de intruções usando os padrões dados na documentação-}

-- | Função principal pedida pelo exercicio
compactMaze :: Maze           -- ^ Labirinto a ser  compactado
            -> Instructions   -- ^ Labirinto Compactado
compactMaze [] = []
compactMaze l = checkRepeats (map buildSingleInstruct l) 0

-- | Função que compacta um corredor numa instrução.
{- |  Esta função compacta os padrões simples e os padrões horizontais 
referidos na documentação da tarefa 3 dentro e um só corredor.-}
buildSingleInstruct :: Corridor     -- ^ Corredor que vai ser convertido a instrução
                    -> Instruction  -- ^ Instrução que é criada
buildSingleInstruct [x] = Instruct [(1, x)]
buildSingleInstruct (h : t)
  | h == x2 = Instruct ((x1 + 1, x2) : xs)
  | otherwise = Instruct ((1, h) : ((x1, x2) : xs))
  where Instruct ((x1, x2) : xs) = buildSingleInstruct t

{- | Função que pega numa lista de instruções e que compacta a lista com
    os padroes verticais descritos na documentação -}
checkRepeats :: Instructions -- ^ Lista de instruções a verificar
             -> Int          -- ^ Indice atual na lista de instruções
             -> Instructions -- ^ Lista de instruções pronta
checkRepeats [] _ = []
checkRepeats (Repeat x : xs) i = Repeat x : checkRepeats xs (i + 1)
checkRepeats (x : xs) i = x : checkRepeats (map (\e -> if e == x then Repeat i else e) xs) (i + 1)

-- * Funções referentes a dar cores às peças do maze

-- | Função que dado uma maze recebe as instruções para alterar as cores
fromMazeToColorInstruct :: Maze -> ColorInstructions
fromMazeToColorInstruct m = getColorInstruct $ compactMaze m

-- | Função que converte instruções para compactação de maze em 
-- | instruções para colorir o maze
getColorInstruct :: Instructions -> ColorInstructions
getColorInstruct l
  = map ( \case Instruct ins -> map lambda ins
                Repeat i -> map lambda (getInstructContent (elemAt l i))) l     
  where lambda = \ (n,piece) -> (concat $ replicate n (show piece),getPieceColor piece)
        getInstructContent (Instruct i) = i

-- | Atribuição de uma cor a cada peça do maze
getPieceColor :: Piece -> CustomColor 
getPieceColor Wall     = None
getPieceColor Empty    = None
getPieceColor (Food z) = Green
getPieceColor (PacPlayer ( Pacman ( PacState (i, c, x, y,z,l) o m Normal ) ) ) = Yellow
getPieceColor (PacPlayer ( Pacman ( PacState (i, c, x, y,z,l) o m Mega   ) ) ) = Red
getPieceColor (PacPlayer ( Pacman ( PacState (i, c, x, y,z,l) o m Dying   ) ) ) = Blue
getPieceColor (PacPlayer (Ghost z) ) = Purple
