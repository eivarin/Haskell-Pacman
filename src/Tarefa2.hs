module Tarefa2 where
import Types
import FileUtils
import Tarefa1

-- * Função referente à primeira fase da função play e as auxiliares a esta

startingState :: State
startingState = loadMaze "maps/2.txt"

ghostInsideHouseId :: Int
ghostInsideHouseId = 2

houseCoords :: Coords
houseCoords = getPlayerCoords (getPlayerByID startingState ghostInsideHouseId)


{- | Primeira fase da função play (correspondente ao player ter ou não a orientação da próxima jogada)
  
  Nota: Se o id não corresponder a nenhum jogador ou o id corresponder a um fantasma devolve o estado original
  -}
play :: Play -> State -> State
play (Move id or) (State m js l)
  | not (elemID id js)               = State m js l
  | not (isPacOrNot (head js'))      = exeGhostPlay (State m cjs l)
  | otherwise                        = playPacGhosts (State m cjs l)          
     where js' = selectPlayer id js
           cjs = changeOrientation or js'

-- | Verifica se o id recebido na função play corresponde a algum jogador da lista
elemID :: Int  -- ^ o ID que queremos ver se existe na lista de todos os players
          -> [Player] -- ^ A lista de todos os players
          -> Bool -- ^ Resultado de percorrer a lista e verificar se existe
elemID _ [] = False
elemID n (Pacman (PacState (id, _, _, _, _, _) _ _ _) : t)
  | n == id   = True 
  | otherwise = elemID n t  
elemID n (Ghost (GhoState (id, _, _, _, _, _) _) : t)
  | n == id   = True
  | otherwise = elemID n t  

{- | Põe o player com id correspondente no inicio da lista de players
   Nota: Nesta função se o ID não existir na lista gera um resultado infinito, mas como se aplica
primeiro a elemID (definida acima) na função play esta só se realiza se o ID existir e por isso 
não vai returnar um resultado infinito -}
selectPlayer :: Int -- ^ ID correspondente ao player selecionado 
                -> [Player] -- ^ Lista de todos os players
                -> [Player] -- ^ Lista de players com o player selecionado pelo ID à cabeça da lista
selectPlayer id (h@(Ghost (GhoState (id', _, _, _, _, _) _)) : t) 
 = if id == id' then h:t else selectPlayer id (t ++ [h])
selectPlayer id (h@(Pacman (PacState (id', _, _, _, _, _) _ _ _)) : t) 
 = if id == id' then h:t else selectPlayer id (t ++ [h])

-- | Verifica se o jodador é ou não o Pacman
isPacOrNot :: Player -> Bool 
isPacOrNot (Ghost gs)  = False
isPacOrNot (Pacman ps) = True

-- | Verifica se a orientação atual é diferente à da próxima jogada
compareOrientation :: Orientation -- ^ A próxima orientação 
                      -> Player -- ^ O orientação atual do player
                      -> Bool -- ^ Resultado de verificar se são diferentes
compareOrientation or' (Pacman (PacState (_, _, _, or, _, _) _ _ _)) = or' /= or
compareOrientation or' (Ghost (GhoState (_, _, _, or, _, _) _))      = or' /= or

-- | Muda a orientação do player 
changeOrientation :: Orientation -- ^ Orientação a atribuir
                     -> [Player] -- ^ Lista de players com o player a atribuir a orientação à sua cabeça
                     -> [Player] -- ^ Resultado 
changeOrientation or' (Pacman (PacState (id, (y,x), v, _, p, nv) tm mouth mode):t) 
 = Pacman (PacState (id, (y,x), v, or', p, nv) tm mouth mode):t
changeOrientation or' (Ghost (GhoState (id, (y,x), v, _, p, nv) gmode):t) 
 = Ghost (GhoState (id, (y,x), v, or', p, nv) gmode):t 

-- * Segunda fase da função play e as auxiliares a esta

{- | Segunda fase da função play, em que se verifica se na próxima posição existe fantasmas ou se o Pacman entra num túnel
    e aplica-se o respetivo efeito -}
playPacGhosts :: State -> State
playPacGhosts s@(State m (k@(Pacman (PacState (id, (y,x), v, or, p, nv) tm mouth mode)) : f) l)
  | isTunnel m coord              = State m (Pacman (PacState (id, lim, v, or, p, nv) tm mouth mode) : f) l
  | isPacDeadAfter k f && nv > 0  = playPacFood (State m (Pacman (PacState (id, (y,x), v, or, p + eat, nv) tm mouth mode) : f) l)
  | isPacDeadAfter k f && nv == 0 = playPacFood (State m (Pacman (PacState (id, (y,x), v, or, p + eat, nv) tm mouth Dying) : f) l)
  | doesPacEatAfter k f           = playPacFood (State m (Pacman (PacState (id, (y,x), v, or, p + eat, nv) tm mouth mode) : change) l)
  | otherwise                     = playPacFood s
  where
    coord  = nextPosition or (y,x)
    lim    = tpTunnel m (y,x)
    change = returnGhostsHome m k f
    eat    = eatDeadGhosts k f

-- | Devolve as coordenadas em função da orientação em que se desloca    
nextPosition :: Orientation -- ^ A orientação para onde se desloca o player 
                -> Coords -- ^ As coordenadas atuais do player
                -> Coords -- ^ As coordenadas resultantes de se movimentar na orientação para onde se desloca 
nextPosition U (y, x) = (y-1, x)
nextPosition D (y, x) = (y+1, x)
nextPosition L (y, x) = (y, x-1)
nextPosition R (y, x) = (y, x+1)

-- * Funções relacionadas aos túneis

-- | Verifica se as coordenadas correspondem a um túnel
isTunnel :: Maze -> Coords -> Bool
isTunnel m (_,x) = x < 0 || x > -1 + length (head m)

-- | Altera as coordenadas do pacman conforme o lado em que entra no túnel
tpTunnel :: Maze -- ^ Maze onde os jogadores estão inseridos
         -> Coords -- ^ coordenada onde o player se encontra antes de passar o túnel
         -> Coords -- ^ coordenada resultante de passar o túnel
tpTunnel m (y,x) | x == 0    = (y,-1 + length (head m))
                 | otherwise = (y,0)

-- * Funções relacionadas aos fantasmas

-- | Verifica se existe um Ghost ,na próxima posição do Pacman, que pode matar o pacman
isPacDeadAfter :: Player -- ^ O jogador escolhido para realizar a jogada
                  -> [Player] -- ^ Lista dos restantes jogadores
                  -> Bool  -- ^ Resultado
isPacDeadAfter _ [] = False
isPacDeadAfter p@(Pacman ps) (Pacman ps1:t) 
  = isPacDeadAfter p t
isPacDeadAfter p@(Pacman (PacState (_, c, _, or, _, _) _ _ _)) (Ghost (GhoState (_, c', _, _, _, _) gmode) : t) 
  = nextPosition or c == c' && getsEaten || isPacDeadAfter p t
  where getsEaten = gmode == Scatter || gmode == Chase || gmode == InHouse

-- | Mesma coisa que isPacDeadAfter mas verifica se existe ghost 
-- | ,na próxima posição do Pacman, que possa ser comido
doesPacEatAfter :: Player -- ^ O jogador escolhido para realizar a jogada
                  -> [Player] -- ^ Lista dos restantes jogadores
                  -> Bool  -- ^ Resultado
doesPacEatAfter _ [] = False
doesPacEatAfter p@(Pacman ps) (Pacman ps1:t) 
  = doesPacEatAfter p t
doesPacEatAfter p@(Pacman (PacState (_, c, _, or, _, _) _ _ _)) (Ghost (GhoState (_, c', _, _, _, _) gmode) : t) 
  = nextPosition or c == c' && gmode == Frightened || doesPacEatAfter p t


-- | Muda o estado de todos os fantasmas que o Pacman tenha comido numa jogada
returnGhostsHome :: Maze -- ^ O maze onde os jogadores estão inseridos (necessário para calcular as coordenadas da casa) 
                    -> Player -- ^ O player que realiza a jogada (necessário para comparar a sua coord com os restasntes players)
                    -> [Player] -- ^ Lista dos restantes jogadores
                    -> [Player] -- ^ Lista dos restantes jogadores após alterar o estado dos fantasmas que foram comidos
returnGhostsHome _ (Pacman ps) [] = []
returnGhostsHome m k@(Pacman ps) (p@(Pacman ps1):t) 
  = p : returnGhostsHome m k t
returnGhostsHome m k@(Pacman (PacState (_, (y,x), _, or, _, _) _ _ _)) (Ghost (GhoState (id', c, v', or', p', nv') gmode) : t)
  | coord == c = Ghost (GhoState (id', middle, 1, or', p', nv') InHouse) : returnGhostsHome m k t
  | otherwise  = Ghost (GhoState (id', c, v', or', p', nv') gmode) : returnGhostsHome m k t
  where
    middle = (div (length m) 2,div (length $ head m) 2)
    coord  = nextPosition or (y,x)

-- | Devolve a quantidade de pontos correspondentes à quantidade de fantasmas que o Pacman irá comer na próxima jogada
eatDeadGhosts :: Player -- ^ O pacman escolhido para realizar a jogada 
                 -> [Player] -- ^ A lista dos restantes players 
                 -> Int -- ^ Os pontos resultantes de o pacman comer (ou não) algum fantasma
eatDeadGhosts _ [] = 0 
eatDeadGhosts k@(Pacman (PacState (_, (y,x), _, or, _, _) _ _ _)) (Ghost (GhoState (_, c, _, _, _, _) gmode):t)
  | (c' == c) && (gmode == Frightened ) = 10 + eatDeadGhosts k t 
  | otherwise                    = 0 + eatDeadGhosts k t
 where c' = nextPosition or (y,x)
eatDeadGhosts k@(Pacman ps) (_:t) = 0 + eatDeadGhosts k t

-- * Funções relacionadas à ultima fase da função play 

-- | Fase final da função play, em que o pacman muda de coordenadas (ou não) e se atribui os respetivos efeitos da próxima peça
playPacFood :: State -> State 
playPacFood s@(State m pac@(Pacman (PacState (id, (y,x), v, or, p, nv) tm mouth mode) : f) l)
  | isPiece m Empty coord         = State newMaze (Pacman (PacState (id, coord, v, or, p, nv) tm mouth mode) : f) l
  | isPiece m (Food Little) coord = State newMaze (Pacman (PacState (id, coord, v, or, p + 1, nv) tm mouth mode) : f) l
  | isPiece m (Food Big) coord    = State newMaze (Pacman (PacState (id, coord, 2, or, p + 5, nv) 24 mouth Mega) : mega) l
  | otherwise                     = s {maze = newMaze}
  where
    coord = nextPosition or (y,x)
    mega  = changeGhostsToMega f
    newMaze = replaceElemInMaze (y,x) Empty m

-- | Verifica se as coordenadas correspondem a uma certa peça 
isPiece :: Maze  -- ^ O maze onde se encontra todas as peças 
          -> Piece -- ^ A peça que queremos verificar se existe
          -> Coords -- ^ As coordenadas onde estamos a verificar se existe a peça escolhida
          -> Bool -- ^ Resultado
isPiece m p (y, x)
  | x < 0 || x > length (head m) - 1 = False
  | elemAt (elemAt m y) x == p         = True
  | otherwise                    = False

-- | Aplica a todos os fantasmas o efeito correspondente ao Pacman estar em modo Mega
changeGhostsToMega :: [Player] -- ^ Lista de todos os jogadores exceto o escolhido para a jogada 
                      -> [Player] -- ^ Resultado de mudar todos os fantasmas após o Pacman comer a Big Food
changeGhostsToMega [] = []
changeGhostsToMega (p@(Pacman _):t) 
  = p : changeGhostsToMega t
changeGhostsToMega (Ghost p@(GhoState _ InHouse ) : t) 
  = Ghost p : changeGhostsToMega t
changeGhostsToMega (player@(Ghost (GhoState (id, (x, y), v, or, p, nv) _)) : t) 
  = Ghost (GhoState (id, (x, y), 0.5, turnBack player, p, nv) Frightened ) : changeGhostsToMega t

turnBack :: Player -> Orientation
turnBack p = case getPlayerOrientation p of U -> D
                                            D -> U
                                            L -> R
                                            R -> L 

exeGhostPlay :: State -> State
exeGhostPlay s@(State m (p@(Ghost (GhoState gs mode)):t) lv) 
  | mode == InHouse && or == U = ghostMovement (State m (Ghost (GhoState gs Scatter):t) lv)
  | coords == houseCoords = ghostMovement (State m (Ghost (GhoState gs InHouse):t) lv)
  | otherwise = ghostMovement s
  where or = getPlayerOrientation p
        coords = getPlayerCoords p

ghostMovement :: State -> State
ghostMovement (State m (k@(Ghost (GhoState  (id, (y,x), v, or, p, nv) mode)) : f) l)
  | isPacAfterMega       = State m (Ghost (GhoState (id, middle, v, or, p, nv) InHouse) : t) l
  | isPacAfterNormal     = State m (Ghost (GhoState (id, coord, v, or, p, nv) mode) : t') l
  | isPiece m Wall coord = State m (Ghost (GhoState (id, (y,x), v, or, p, nv) mode) : t) l
  | isTunnel m coord     = State m (Ghost (GhoState (id, lim, v, or, p, nv) mode) : t) l
  | otherwise            = State m (Ghost (GhoState (id, coord, v, or, p, nv) mode) : t) l
  where coord = nextPosition or (y,x)
        lim   = tpTunnel m (y,x)
        middle = (div (length m) 2,div (length $ head m) 2)
        t@(pac@(Pacman _):f') = selectPlayer 0 f
        pacCoords = getPlayerCoords pac
        t' = ghostHitsPacman pac : f'
        coordsCheck      =  coord == pacCoords || (y,x) == pacCoords
        isPacAfterMega   =  coordsCheck && mode == Frightened                  
        isPacAfterNormal =  coordsCheck && (mode == Scatter || mode == Chase || mode == InHouse)                  

ghostHitsPacman :: Player -> Player 
ghostHitsPacman p@(Pacman ps) 
  | nv > 1 = setPlayerLives p (nv-1)
  | otherwise = setPlayerLives (setPlayerMode p (Left Dying)) (nv-1) 
  where nv = getPlayerLives p
