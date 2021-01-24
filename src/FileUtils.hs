module FileUtils where

import System.IO.Unsafe
import Types -- vosso ficheiro com definicao de tipos de dados


loadMaze :: String -> State
loadMaze filepath = unsafePerformIO $ readStateFromFile filepath


readStateFromFile :: String -> IO State
readStateFromFile f = do
                content <- readFile f
                let llines = lines content 
                    (new_map,pl) = convertLinesToPiece llines 0 []
                return (State new_map pl 1)



convertLinesToPiece :: [String] -> Int -> [Player] -> (Maze,[Player])
convertLinesToPiece [] _ l = ([],l)
convertLinesToPiece (x:xs) n l  = let (a,b) = convertLineToPiece x n 0 l 
                                      (a1,b1) = convertLinesToPiece xs (n+1) b
                                  in (a:a1,b1)

convertLineToPiece :: String -> Int -> Int -> [Player] -> ([Piece],[Player])
convertLineToPiece [] _ _ l = ([],l)
convertLineToPiece (z:zs) x y l = let (a,b ) = charToPiece z x y l
                                      (a1,b1) = convertLineToPiece zs x (y+1) b 
                                  in (a:a1,b1)

--mudancas para por pacman c id 0
charToPiece :: Char -> Int -> Int -> [Player] -> (Piece,[Player])
charToPiece c x y l
    | c == '{' =  (Empty, Pacman ( PacState (0, (x,y), 1, R,0,10 ) 0 Open Normal  ): makePacmanFstIdAux l)
    | c == '<' =  (Empty, Pacman ( PacState (0, (x,y), 1, R,0,10 ) 0 Closed Normal): makePacmanFstIdAux l)
    | c == '}' =  (Empty, Pacman ( PacState (0, (x,y), 1, L,0,10 ) 0 Open Normal  ): makePacmanFstIdAux l)
    | c == '>' =  (Empty, Pacman ( PacState (0, (x,y), 1, L,0,10 ) 0 Closed Normal): makePacmanFstIdAux l)
    | c == 'V' =  (Empty, Pacman ( PacState (0, (x,y), 1, U,0,10 ) 0 Open Normal  ): makePacmanFstIdAux l)
    | c == 'v' =  (Empty, Pacman ( PacState (0, (x,y), 1, U,0,10 ) 0 Closed Normal): makePacmanFstIdAux l)
    | c == '^' =  (Empty, Pacman ( PacState (0, (x,y), 1, D,0,10 ) 0 Open Normal  ): makePacmanFstIdAux l)
    | c == '|' =  (Empty, Pacman ( PacState (0, (x,y), 1, D,0,10 ) 0 Closed Normal): makePacmanFstIdAux l)
    | c == 'X' =  (Empty, Pacman ( PacState (0, (x,y), 1, R,0,10 ) 0 Open Dying   ): makePacmanFstIdAux l)
    | c == 'M' =  (Empty, Ghost  ( GhoState (length l,(x,y),1,R,0,1 ) InHouse    ): l ) 
    | c == '?' =  (Empty, Ghost  ( GhoState (length l,(x,y),1,U,0,1 ) Frightened ): l )
    | c == 'o' =  (Food Big,l)
    | c == '.' =  (Food Little,l)
    | c == '#' =  (Wall,l)
    | otherwise = (Empty,l)

-- | Função que faz com que o pacman tenha id sempre 0 por meio de adicionar 
-- | 1 a todos os ids dos ghosts visto q so existe 1 pacman
makePacmanFstIdAux :: [Player] -> [Player]
makePacmanFstIdAux [] = []
makePacmanFstIdAux (Ghost ( GhoState (id,coor, v, o, p, l ) gm):t)
    = Ghost ( GhoState (id+1,coor, v, o, p, l ) gm) : makePacmanFstIdAux t