{- |
= Introdução
   A Tarefa 6 consiste em fazer com que o pacman conlua o jogo realizando e avaliando jogadas.
= Objetivos
   Para nós os objetivos e a estratégia adotada para fazer um bot que movimenta o pacman sozinho 
foram avaliar as peças de acordo com uma tabela de pontuação, em que uma comida vale mais e 
um fantasma vale menos, por exemplo. Também uma das estratégias foi criar uma Rose Tree em que
cada MappedPlay, possível jogada, estaria associada a uma layer ou camada e que a essa mappedplay estaria associada
uma lista de mappedplays, possíveis jogadas após a realização da mesma, para essa e associadas também a 
uma layer mais pequena, quanto mais baixa fosse a layer menos importaria ou influenciaria a tomada de decisão
do bot de acordo com o sistema de pontuação.
= Discussão e conclusão
 Apesar de termos conseguido a implementação de todos os objetivos não conseguimos fazer com que 
 o pacman finalizasse o jogo pois haveria um momento em que entraria em um loop à volta de um obstáculo
 do mapa, concluimos que tal coisa poderia ser resultante ou do método inefeciente que escolhemos para
 relizar a tarefa ou da performance do haskell, pois neste último caso tivemos que usar apenas 10 layers (fez 
 com que o bot tivesse uma percentagem de pouco acerto nas jogadas que decidia) de jogadas
 para o bot avaliar pois mais que isso fazia com que o jogo se tornasse muito lento por estar a 
 calcular um número elevado de possibilidades. 
-}

module Tarefa6 where 

import Types
import Tarefa2
import Tarefa5

-- | Função objetivo que devolve a melhor play para alcançar o objetivo de finalizar o jogo
bot :: Int             -- ^ Id do pacman
       -> State        -- ^ State atual do jogo
       -> Maybe Play   -- ^ Play que o pacman irá realizar após avaliação de várias outras jogadas
bot 0 s = Just $ functionUnion s
bot x s = Nothing

-- | Número de jogadas à frente que o pacman avalia
numberOfLayers :: Int
numberOfLayers = 10

-- | Função que junta todas as auxiliares e devolve a play com a melhor pontuação
functionUnion :: State -> Play
functionUnion s = Move 0 or
                where ns = foldr (\x st-> foldr play st (ghostPlay st)) s [0..numberOfLayers]
                      p = getPlayerByID s 0 
                      gh = PacPlayer (getPlayerByID s 1)
                      m = placePlayersOnMap (playersState s) (maze s)
                      orCoords = (getPlayerOrientation p, getPlayerCoords p)
                      mpl = getMappedPlayList (getPlaysList1 m orCoords) m 0
                      pacM = getPacmanMode p
                      nl1 = maximumsFst $ map (\mp -> (getRouteScore mp maximum ,mp)) mpl
                      or = if null nl1 then turnBack p else nextOr (snd (head nl1))

-- | Função que devolve uma lista de mappedplay
getMappedPlayList :: [(Orientation, Coords)] -> Maze -> Int -> [MappedPlay]
getMappedPlayList [] _ _ = []
getMappedPlayList l m layer = map (\x -> getMappedPlay x m layer) l
-- | Função que devolve uma mappedplay 
getMappedPlay :: (Orientation, Coords)  -- ^ Par da próx orientação e próx coordenada
                 -> Maze                -- ^ Maze
                 -> Int                 -- ^ Número da layer
                 -> MappedPlay          -- ^ Mappedplay resultante
getMappedPlay (or,coords) m layer
  | layer == numberOfLayers = MappedPlay layer or coords nextPiece []
  | otherwise  = MappedPlay layer or coords nextPiece mappedPlaysList
  where nextPiece = getPieceFromMaze coords m
        newLayer = layer + 1
        possiblePlays = getPlaysList1 m (or,coords)
        mappedPlaysList = getMappedPlayList possiblePlays m newLayer  
-- | Função que devolve a pontuação de uma determinada mappedplay
-- | Quanto maior a pontuação, melhor o caminho para o bot
getRouteScore :: MappedPlay              -- ^ mappedPlay a avaliar
                 -> ([Double] -> Double) -- ^ função que recebe uma lista de pontuações e devolve uma especifica, irá ser a de maxima ou minima pontuação
                 -> Double                -- ^ pontuação de uma mappedplay  
getRouteScore mp@(MappedPlay _ nOr _ np []) f = evaluatePiece np nOr
getRouteScore mp@(MappedPlay _ nOr _ np _) f = evaluatePiece np nOr + (0.1 * f lDouble)
                                                where mpl = nextPlays mp 
                                                      lDouble = map (`getRouteScore` f) mpl
-- | Função que atribui uma determinada pontuação a cada peça
-- | Sistema útil para determinar as peças com maior importância para a decisão de movimento do bot
evaluatePiece :: Piece -> Orientation -> Double 
evaluatePiece Empty _ = 0
evaluatePiece (Food Little) _ = 1
evaluatePiece (Food Big) _ = 4
evaluatePiece (PacPlayer (Ghost (GhoState _ Frightened ))) _ = 8
evaluatePiece (PacPlayer gh@(Ghost _)) or | getPlayerOrientation gh == or = 1
                                          | otherwise = fromIntegral (-numberOfLayers-10)
evaluatePiece (PacPlayer _) _ = -20

-- | Devolve as mappedPlays com a menor pontuação
minimumsFst :: [(Double, MappedPlay)] -> [(Double, MappedPlay)]
minimumsFst [] = []
minimumsFst xs = filter ((==) minfst . fst) xs
    where minfst = minimum (map fst xs)

-- | Devolve as mappedPlays com maior pontuação
maximumsFst :: [(Double, MappedPlay)] -> [(Double, MappedPlay)]
maximumsFst [] = []
maximumsFst xs = filter ((==) maxfst . fst) xs
    where maxfst = maximum (map fst xs)

-- | Verifica se a peça é um fantasmas
isPieceGhost :: Piece -> Bool 
isPieceGhost (PacPlayer (Ghost g)) = True
isPieceGhost p = False

-- | Verifica se a peça é uma comida
isPieceFood :: Piece -> Bool
isPieceFood (Food _) = True
isPieceFood _ = False

-- | Devolve a peça com determinadas coordenadas de um maze
getPieceFromMaze :: Coords -> Maze -> Piece 
getPieceFromMaze (y,x) m = elemAt (elemAt m y) x

