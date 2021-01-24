{- |
= Introdução
   A Tarefa 5 consiste em fazer com que os fantasmas persigam o pacman ou fujam dele, quando está em modo Mega, ou seja criar os movimentos deles.
= Objetivos
Para nós os objetivos foram:
 * Criar bots para os fantasmas em que cada um tivesse uma estratégia diferente 
de perseguir o pacman, assim como no jogo original.
 * Criar 4 tipos de modos para os fantasmas: 
   * o modo Frightened , em que o objetivo dos fantasmas é fugir do pacman estando “mortos”; 
   * o modo Chase, em que cada um dos 4 fantasma persegue-o de acordo com a sua determinada estratégia; 
   * o Modo Scatter, em que cada um dos quatro fantasmas move-se para cada 
um dos diferentes cantos e circula à volta do obstáculo mais próximo; 
   e por fim o modo InHouse em que o objetivo é fazer com que o fantasma saia da casa.
 * Os diferentes fantasmas, aos quais nomeamos Blinky, Pinky, Inky e Clyde, 
teriam que ter um perfil diferente em Chase mode, ou seja, a Blinky perseguiria o pacman de 
acordo com as suas coordenadas, a Pinky perseguiria o pacman de acordo com 4a coordenada 
à frente do pacman, a Inky perseguiria a coordenada do objetivo da Blinky multiplicada por 2 e 
a Clyde perseguiria as coordenadas do pacman se tivesse a uma distância menor ou igual a 8 caso 
contrário reagiria como se estivesse em Scatter mode.
= Discussão e conclusão
 Apesar de termos atingido todos os objetivos achamos que os bots estão num nível de dificuldade elevada
para o jogador
-}

module Tarefa5 where 

import Tarefa2
import System.Random

import Types

-- | Função objetivo desta tarefa que ao receber o estado atual do jogo devolve a lista de melhores plays
-- | para cada fantasma de modo a reagir ao pacman
ghostPlay :: State -> [Play]
ghostPlay s@(State _ (h:t) _) = decideGhostsActions t s 

-- | Devolve a lista plays que os fantasmas irão realizar de acordo com o seu modo atual
decideGhostsActions :: [Player] -> State -> [Play]
decideGhostsActions l s 
  = map (\(GhoState(id,_,_,_,_,_) m) -> case m of Scatter    -> scatterMode s id
                                                  Chase      -> chaseMode s id 
                                                  Frightened -> frightenedMode s id
                                                  InHouse    -> inHouseMode s id ) (removePacFromList l)
-- | Remove o pacman da lista de players 
removePacFromList :: [Player] -> [GhoState]
removePacFromList [] = []
removePacFromList ((Ghost gs@(GhoState _ m)):l) = gs : removePacFromList l
removePacFromList ((Pacman _):l) = removePacFromList l
-- | Recebe o modo do fantasma
getGhostMode :: Player -> GhostMode 
getGhostMode (Ghost (GhoState _ m )) = m

-- | Função que devolve a play que permite aos fantasmas quando estão vivos 
-- | e em modo chase seguirem o pacman, cada um com uma estratégia diferente
chaseMode :: State -> Int -> Play
chaseMode s id =  case profile of 1 -> profileBlinky s id
                                  2 -> profilePinky s id
                                  3 -> profileInky s id
                                  4 -> profileClyde s id
                  where profile = mod id 4 + 1
-- | Perfil do fantasma, Blinky, com perfil 1, que realiza plays em função do objetivo ser 
-- | a coordenada do pacman
profileBlinky :: State -> Int -> Play
profileBlinky s id =  let m  = maze s
                          ghost  = getPlayerByID s id
                          (orientPac, coordsPac) = getPacOrAndCoor (getPlayerByID s 0)
                          in getPlay ghost m coordsPac
-- | Perfil do fantasma, Pinky, com perfil 2, que realiza plays em função do seu objetivo ser
-- | a quarta coordenada à frente do pacman de acordo com a orientação do mesmo
profilePinky :: State -> Int -> Play
profilePinky s id = let m  = maze s
                        ghost  = getPlayerByID s id
                        pacLocationInfo = getPacOrAndCoor (getPlayerByID s 0)
                        objective = translateCoordsByOr pacLocationInfo 4
                        in getPlay ghost m objective
-- | Perfil do fantasma, Inky, com perfil 3, que realiza plays em função do seu objetivo ser 
-- | a coordenada que corresponde ao ponto do dobro da distância entre o pacman 
-- | e do fantasma, Blinky 
profileInky :: State -> Int -> Play
profileInky s id =  let m  = maze s
                        ghost  = getPlayerByID s id
                        blinkyLoc = getPlayerCoords  (getPlayerByID s 1)
                        pacLocationInfo = translateCoordsByOr (getPacOrAndCoor (getPlayerByID s 0)) 2
                        objective = getPointOfDoubleDistance blinkyLoc pacLocationInfo
                        in getPlay ghost m objective

-- | Perfil do fantasma, Clyde, com perfil 4, que realiza plays consoante a distância
-- | ao pacman, se for maior que 8 tem o mesmo objetivo do fantasma Blinky caso contrário
-- | tem como objetivo o mesmo que teria no modo Scatter
profileClyde :: State -> Int -> Play
profileClyde s id | distToPacman > 8 = profileBlinky s id
                  | otherwise = scatterMode s id
                  where distToPacman           = distanceOnPlain (coordsGhost, coordsPac)
                        ghost                  = getPlayerByID s id
                        coordsGhost            = getPlayerCoords ghost
                        (orientPac, coordsPac) = getPacOrAndCoor (getPlayerByID s 0)

-- | De acordo com um jogador, um maze e um objetivo devolve a melhor play 
-- | para atingir o objetivo pretendido
getPlay :: Player -> Maze -> Coords -> Play
getPlay p m goal  = Move id or 
                  where id = getPlayerID p
                        or = if null plays then turnBack p else getBestPlay plays goal 
                        plays = getPlaysList m p


-- | Devolve a orientação da próxima melhor coordenada para o player se mover
-- | de acordo com a menor distância da mesma até ao objetivo
getBestPlay ::  [(Orientation, Coords)] -- ^ lista da orientação para a próxima coordenada em par
                -> Coords -- ^ coordenada do objetivo (para onde o player pretende ir)
                -> Orientation -- ^ melhor orientação para atingir o objetivo
getBestPlay [(or,_)] _ = or
getBestPlay (h:t) goal 
  = or
  where (or,distance) = foldr (\ (x1,x2) y@(y1,y2) -> if distanceOnPlain (x2, goal) < y2
                                        then (x1, distanceOnPlain (x2, goal))
                                        else y) (fst h, distanceOnPlain (snd h, goal)) t

-- | Calcula a distância no plano de uma coordenada a outra
distanceOnPlain :: (Coords,Coords) -> Float
distanceOnPlain ((y1,x1),(y2,x2)) = sqrt (fromIntegral ((y2-y1)^2 + (x2-x1)^2))

-- | Devolve a lista de jogadas possíveis do player sem nunca voltar para trás ou embater numa parede
getPlaysList :: Maze -> Player -> [(Orientation, Coords)]
getPlaysList m p = parseWallPlays m $ getPossiblePlays p

-- | Mesma funcionalidade da getPlaysList mas usada na Tarefa6
getPlaysList1 :: Maze -> (Orientation, Coords) -> [(Orientation, Coords)]
getPlaysList1 m (or,coords) = parseWallPlays m $ getPossiblePlays1 (or, coords)

-- Função que remove todas as jogadas que conduzem a embater com a parede
parseWallPlays :: Maze -> [(Orientation, Coords)] -> [(Orientation, Coords)]
parseWallPlays m [] = []
parseWallPlays m ((or,c):t) | isPiece m Wall c = parseWallPlays m t
                            | isTunnel m c = (or,lim):parseWallPlays m t
                            | otherwise = (or,c):parseWallPlays m t
                            where lim = tpTunnel m c

-- | Função que calcula todas as jogadas possíveis de o jogador realizar sem 
-- | nunca voltar para trás no Maze
getPossiblePlays :: Player                         -- ^ Player em questão
                    -> [(Orientation, Coords)]     -- ^ Lista de todos os pares orientação da próx. jogada e próx coordenada
getPossiblePlays p 
  = case or of 
         U -> [(U, playU),(L, playL),(R, playR)]
         D -> [(D, playD),(L, playL),(R, playR)]
         L -> [(U, playU),(D, playD),(L, playL)]
         R -> [(R, playR),(D, playD),(U, playU)]
  where or = getPlayerOrientation p
        c = getPlayerCoords p
        playU = nextPosition U c
        playD = nextPosition D c
        playL = nextPosition L c
        playR = nextPosition R c

-- | Função com a mesma funcionalidade da getPossiblePlays e que será usada na
-- | tarefa 6
getPossiblePlays1 :: (Orientation,Coords) -> [(Orientation, Coords)] 
getPossiblePlays1 (or,c) 
  = case or of 
         U -> [(U, playU),(L, playL),(R, playR)]
         D -> [(D, playD),(L, playL),(R, playR)]
         L -> [(U, playU),(D, playD),(L, playL)]
         R -> [(R, playR),(D, playD),(U, playU)]
  where playU = nextPosition U c
        playD = nextPosition D c
        playL = nextPosition L c
        playR = nextPosition R c

-- | Função que dado um inteiro e uma orientação altera a coordenada
-- | Útil para definir o objetivo de certos perfis dos Fantasmas
translateCoordsByOr :: (Orientation, Coords) -> Int -> Coords
translateCoordsByOr (or,(y,x)) mod 
  = case or of 
         U -> (y-mod,x)
         D -> (y+mod,x)
         L -> (y,x-mod)
         R -> (y,x+mod)

-- | Dadas duas coordenadas devolve a respetiva coordenada ao ponto do dobro da distância
-- | Função usada para calcular o objetivo Inky, que depende das coords do pac e da Blinky
getPointOfDoubleDistance :: Coords -> Coords -> Coords
getPointOfDoubleDistance (y1,x1) (y2,x2) = (((y1-y2)*2)+y1,((x1-x2)*2)+x1)

-- | Dado um player recebe um par orientação e coordenadas 
getPacOrAndCoor :: Player -> (Orientation, Coords)
getPacOrAndCoor p = (getPlayerOrientation p, getPlayerCoords p)


-- | Dado um state e um perfil de um Fantasma define a coord objetivo quando
-- | se encontra em modo Scatter ou seja cada Fantasma tem correspondência com um canto do maze
scatterMode :: State -> Int -> Play
scatterMode s id = case profile of 1 -> getPlay p m (1,1)
                                   2 -> getPlay p m (1,maxX)
                                   3 -> getPlay p m (maxY,1)
                                   4 -> getPlay p m (maxY,maxX)
                 where m  = maze s
                       p  = getPlayerByID s id
                       profile = mod id 4 + 1
                       maxY = length m - 2
                       maxX = length (head m) - 2

-- | Função que devolve uma play aleatorizada e que é executada
-- | quando os fantasmas estão em modo Frightened
frightenedMode :: State -> Int -> Play
frightenedMode s id = Move id or
                where m  = maze s                           --  maze atual
                      p  = getPlayerByID s id               -- jogador com respetivo id
                      plays = map fst (getPlaysList m p)    -- Lista das orientações possíveis para a próxima play
                      score = getPlayerPoints (getPlayerByID s 0) -- número de pontos atuais do pacman
                      rn = getRandomIndex (length plays - 1) score -- Devolve um índice aleatorizado da lista definida em plays
                      or = elemAtSpec plays rn -- orientação correspondente a escolher um indice aleatório da lista plays
-- | Função que permite aos fantasmas que estão na casa sairem e
-- | que é utilizada quando existirem fantasmas em modo InHouse
inHouseMode :: State -> Int -> Play
inHouseMode s id = getPlay p m (1,1)
               where m  = maze s
                     p  = getPlayerByID s id
-- Devolve um índice aleatorizado 
getRandomIndex :: Int -> Int -> Int 
getRandomIndex i seed  = rn
                        where (rn,f) =  randomR (0, i) (mkStdGen seed)