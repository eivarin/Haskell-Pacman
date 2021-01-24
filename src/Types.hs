module Types where

import Data.List
import UI.NCurses

-- * Data Types do jogo
data State = State 
    {
        maze :: Maze
    ,   playersState :: [Player]
    ,   level :: Int
    }

type Maze = [Corridor]
type Corridor = [Piece]
data Piece =  Food FoodType | PacPlayer Player| Empty | Wall deriving (Eq)
data Player =  Pacman PacState | Ghost GhoState deriving (Eq)

data Orientation = L | R | U | D | Null deriving (Eq,Show)
data PacState= PacState 
    {   
        pacState :: PlayerState
    ,   timeMega :: Double
    ,   openClosed :: Mouth
    ,   pacmanMode :: PacMode
    
    } deriving Eq

data GhoState= GhoState 
    {
        ghostState :: PlayerState
    ,   ghostMode :: GhostMode
    } deriving Eq

type Coords = (Int,Int)
type PlayerState = (Int, Coords, Double , Orientation, Int, Int)
--                 (ID,  (x,y), velocity, orientation, points, lives) 
data Mouth = Open | Closed deriving (Eq,Show)
data PacMode = Dying | Mega | Normal deriving (Eq,Show)
-- | Atribuimos novos modos aos fantasmas para enriquecer o jogo e para a realização da tarefa5
data GhostMode  = Scatter  -- ^ Modo em que o fantasma está vivo e não segue o pacman, indo para um canto do maze
                | Chase -- ^ Modo que representa que o fantasma está vivo e que o seu objetivo é seguir o pacman
                | Frightened -- ^ Modo que representa que o fantasma está morto, pacman modo mega, e que não segue o pacman
                | InHouse -- ^ Modo que indica que o fantasma está na casa e que o seu objetivo é sair da mesma
            deriving (Eq,Show)
data FoodType = Big | Little deriving (Eq)
-- | Um novo data type que nos permitirá pôr o maze colorido
data CustomColor = Blue | Green | Purple | Red | Yellow | None deriving (Eq,Show)

data Play = Move Int Orientation deriving (Eq,Show)

-- * Data Types da Tarefa 6

-- | Um novo data type que definimos para a realização da tarefa6 e que
-- | consiste em uma Rose Tree que permite mapear todas as próximas jogadas do pacman 
data MappedPlay = MappedPlay {
      layer :: Int
    , nextOr :: Orientation
    , nextCoords :: Coords
    , nextPiece :: Piece
    , nextPlays :: [MappedPlay]
} deriving Eq
-- | Instância da classe show da MappedPlay
instance Show MappedPlay where
  show (MappedPlay l nOr nCoords nPiece nPlays)
    = layerSTR ++ orientSTR ++ coordsSTR ++ pieceSTR -- ++ playsSTR
    where layerSTR = "Layer: " ++ show l
          orientSTR = "; NextOrientation: " ++ show nOr
          coordsSTR = "; NextCoord: " ++ show nCoords
          pieceSTR = "; NextPiece: " ++ show nPiece
          playsSTR = foldr (\mp st -> st ++ "\n" ++ concat(replicate l "  ") ++ show mp) "" nPlays

-- * Data Types da Tarefa 3

type Instructions = [Instruction]

data Instruction = Instruct [(Int, Piece)]
                 | Repeat Int deriving (Show, Eq)
-- | Representação de um maze com colorinstruction
type ColorInstructions = [ColorInstruction] 
-- | Representação de um corridor com uma lista de strings e cores
type ColorInstruction = [(String, CustomColor)]

-- * Instâncias da classe show dos Data Types

instance Show State where
  show (State m ps p) = printMaze mz ++ "Level: " ++ show p ++ "\nPlayers: \n" ++ (foldr (++) "\n" (map (\y-> printPlayerStatsDebug y) ps))
                          where mz = placePlayersOnMap ps m
-- | Função usado para representar em forma de string um state sem representar o maze
instStateShowNew :: State -> String
instStateShowNew s@(State m ps p)
  = "Level: " ++ show p ++ "\nPlayers: \n" ++ foldr (++) "\n" (map (\y-> printPlayerStats y) ps)

instance Show PacState where
   show ( PacState s o m Dying  ) =  "X"
   show ( PacState (a,b,c,R,i,l) _ Open m  ) =  "{"
   show ( PacState (a,b,c,R,i,l) _ Closed m  ) =  "<"
   show ( PacState (a,b,c,L,i,l) _ Open m  ) =  "}"
   show ( PacState (a,b,c,L,i,l) _ Closed m  ) =  ">"
   show ( PacState (a,b,c,U,i,l) _ Open m  ) =  "V"
   show ( PacState (a,b,c,U,i,l) _ Closed m  ) =  "v"
   show ( PacState (a,b,c,D,i,l) _ Open m  ) =  "^"
   show ( PacState (a,b,c,D,i,l) _ Closed m  ) =  "|"
   show ( PacState (a,b,c,Null,i,l) _ Closed m  ) =  "<"
   show ( PacState (a,b,c,Null,i,l) _ Open m  ) =  "{"

instance Show Player where
   show (Pacman x ) =  show x
   show ( Ghost x ) =   show x

instance Show GhoState where
   show (GhoState x Frightened ) =  "?"
   show (GhoState x _ ) =  "M"
-- | Alteramos a representação da Food 
instance Show FoodType where
   show ( Big ) =  "•"
   show ( Little ) =  "·"

instance Show Piece where
   show Wall     = "#"
   show Empty    = " "
   show (Food z) = show z
   show (PacPlayer ( Pacman ( PacState (i, c, x, y,z,l) o m Normal ) ) ) = show ( PacState (i, c, x, y,z,l) o m Normal)
   show (PacPlayer ( Pacman ( PacState (i, c, x, y,z,l) o m Mega   ) ) ) = show ( PacState (i, c, x, y,z,l) o m Mega)
   show (PacPlayer ( Pacman ( PacState (i, c, x, y,z,l) o m Dying   ) ) ) = show ( PacState (i, c, x, y,z,l) o m Dying)
   show (PacPlayer (Ghost z) ) = show z



-- | Função que gera os colorID's da curses para cada CustomColor
coloredCursesString ::  CustomColor -> Curses ColorID 
coloredCursesString Red = newColorID ColorRed ColorBlack 1
coloredCursesString Blue = newColorID ColorBlue ColorBlack 2
coloredCursesString None = newColorID ColorWhite ColorBlack 3
coloredCursesString Green = newColorID ColorGreen ColorBlack 4
coloredCursesString Yellow = newColorID ColorYellow ColorBlack 5
coloredCursesString Purple = newColorID ColorMagenta ColorBlack 6


placePlayersOnMap :: [Player] -> Maze -> Maze
placePlayersOnMap [] x = x
placePlayersOnMap (x:xs) m = placePlayersOnMap xs ( replaceElemInMaze (getPlayerCoords x) (PacPlayer x) m )


printMaze :: Maze -> String
printMaze []  =  ""
printMaze (x:xs) = foldr (++) "" ( map (\y -> show y) x )  ++ "\n" ++ printMaze ( xs )

printPlayerStats :: Player -> String
printPlayerStats p = let (a,b,c,d,e,l) = getPlayerState p
                     in "ID:" ++ show a ++  " Points:" ++ show e ++ " Lives:" ++ show l ++"\n"
-- | Função de debug que dá print de todas as caracteríscas de um player
printPlayerStatsDebug :: Player -> String
printPlayerStatsDebug (Pacman (PacState (x,y,z,t,h,l) q c d ))
  ="ID:" ++ show x ++  " Coords:" ++ show y ++ " Veloc:" ++ show z ++ " Orient:" ++ show t ++ " Points:" ++ show h ++ " Lives:" ++ show l ++ " timeMega:" ++ show q ++ " Mouth:" ++ show c ++ " Mode:" ++ show d ++ "\n"
printPlayerStatsDebug (Ghost (GhoState (x,y,z,t,h,l) q ))
  ="ID:" ++ show x ++  " Coords:" ++ show y ++ " Veloc:" ++ show z ++ " Orient:" ++ show t ++ " Points:" ++ show h ++ " Lives:" ++ show l ++ " Mode:" ++ show q ++ "\n"

-- * Funções de acesso e alteração ao player

getPlayerID :: Player -> Int
getPlayerID (Pacman (PacState (id,_,_,_,_,_) _ _ _ )) = id
getPlayerID  (Ghost (GhoState (id,_,_,_,_,_) _ )) = id

getPlayerByID :: State -> Int -> Player
getPlayerByID s id = head $ filter (\ p -> getPlayerID p == id) (playersState s)

getPlayerPoints :: Player -> Int
getPlayerPoints (Pacman (PacState (_,_,_,_,points,_) _ _ _ )) = points
getPlayerPoints (Ghost (GhoState (_,_,_,_,points,_) _ )) = points

setPlayerCoords :: Player -> Coords -> Player
setPlayerCoords (Pacman (PacState (x,_,z,t,h,l) q c d )) (a,b) = Pacman (PacState (x,(a,b),z,t,h,l) q c d )
setPlayerCoords (Ghost (GhoState (x,_,z,t,h,l) q )) (a,b) = Ghost (GhoState (x,(a,b),z,t,h,l) q )

getPlayerLives :: Player -> Int
getPlayerLives (Pacman (PacState (_,_,_,_,_,lives) _ _ _ )) = lives
getPlayerLives (Ghost (GhoState (_,_,_,_,_,lives) _ )) = lives

setPlayerLives :: Player -> Int -> Player
setPlayerLives (Pacman (PacState (x,y,z,t,h,_) q c d )) newLives 
  = Pacman (PacState (x,y,z,t,h,newLives) q c d )
setPlayerLives (Ghost (GhoState (x,y,z,t,h,_) q )) newLives 
  = Ghost (GhoState (x,y,z,t,h,newLives) q )

setPlayerMode :: Player -> Either PacMode GhostMode -> Player
setPlayerMode (Pacman p) (Left mode) = Pacman p {pacmanMode = mode}
setPlayerMode (Ghost g) (Right mode) = Ghost g {ghostMode = mode}

getPlayerSpeed :: Player -> Double
getPlayerSpeed (Pacman (PacState (_,_,vel,_,_,_) _ _ _ )) = vel
getPlayerSpeed (Ghost (GhoState (_,_,vel,_,_,_) _ )) = vel

setPlayerSpeed :: Player -> Double -> Player
setPlayerSpeed (Pacman (PacState (x,y,z,t,h,l) q c d )) newSpeed 
  = Pacman (PacState (x,y,newSpeed,t,h,l) q c d )
setPlayerSpeed (Ghost (GhoState (x,y,z,t,h,l) q )) newSpeed 
  = Ghost (GhoState (x,y,newSpeed,t,h,l) q )

getPieceOrientation :: Piece -> Orientation
getPieceOrientation (PacPlayer p) =  getPlayerOrientation p
getPieceOrientation _ = Null

getPacMode :: Player -> PacMode
getPacMode (Pacman (PacState a b c d)) = d

getPacmanMode :: Player -> PacMode
getPacmanMode (Pacman (PacState a b c d)) = d

getPlayerState :: Player -> PlayerState
getPlayerState (Pacman (PacState a b c d )) = a
getPlayerState (Ghost (GhoState a b )) = a

getPlayerOrientation :: Player -> Orientation
getPlayerOrientation (Pacman (PacState (x,y,z,t,h,l) q c d )) = t
getPlayerOrientation  (Ghost (GhoState (x,y,z,t,h,l) q )) = t

getPacTimeMega :: Player -> Double
getPacTimeMega (Pacman p) = timeMega p

setPacMegaTime :: Player -> Double -> Player
setPacMegaTime (Pacman p) nTM = Pacman p {timeMega = nTM}

replaceElemInMaze :: Coords -> Piece -> Maze -> Maze
replaceElemInMaze (a,b) _ [] = []
replaceElemInMaze (a,b) p (x:xs) 
  | a == 0 = replaceNElem b p x : xs 
  | otherwise = x : replaceElemInMaze (a-1,b) p xs


replaceNElem :: Int -> a -> [a] -> [a]
replaceNElem i _ [] = [] 
replaceNElem i el (x:xs)
  |  i == 0 = el : xs 
  | otherwise =  x : replaceNElem (i-1) el xs

getPlayerCoords :: Player -> Coords
getPlayerCoords (Pacman (PacState (x,y,z,t,h,l) b c d )) = y
getPlayerCoords (Ghost (GhoState (x,y,z,t,h,l) b )) = y

flipPacMouth :: Player -> Player
flipPacMouth (Pacman p) 
  = Pacman p {openClosed = nMouth}
  where nMouth = if openClosed p == Open then Closed else Open

-- * Outras funções

elemAt :: [a] -> Int -> a
elemAt [] i = error ("indice:" ++ show i) 
elemAt (h:t) 0 = h
elemAt (h:t) i = elemAt t (i-1)

elemAtSpec :: [Orientation] -> Int -> Orientation
elemAtSpec [] i = error ("indice++:" ++ show i)
elemAtSpec (h:t) 0 = h
elemAtSpec (h:t) i = elemAtSpec t (i-1)