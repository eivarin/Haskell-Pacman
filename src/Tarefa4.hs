{- |
= Introdução
  A Tarefa 4 consistiu em atualizar um estado de jogo, que até então, tarefa 2, era atualizado por via de executar a função play, 
  com base no efeito da passagem de um instante de tempo e que atualizasse não um, mas todos os jogadores e o mapa.
= Objetivos
  Os principais objetivos foram criar as funções que se relacionavam diretamente com o tempo, ou seja, 
  atualizar o número de iterações de acordo com a velocidade do jogador, atualizar o pacman de acordo com o controlo do seu 
  movimento com as setas e atualizar os fantasmas de acordo com o modo do pacman e com o step atual do jogo, 
  tentando sempre fazer com que se parecesse ao jogo original.
= Discussão e conclusão
Conseguimos atingir todos os objetivos 
-}

module Tarefa4 where 
import Tarefa2 
import Tarefa5
import Tarefa6
import Types



defaultDelayTime = 250

-- | Steps quando os fantasmas entram em modo Scatter
switchToScatterTimmings = [88,186,304]
-- | Steps quando os fantasmas entram em modo Chase
switchToChaseTimmings = [28,126,244,404]

-- | Função objetivo que permite fazer atualizações ao estado com o efeito da passagem de uma intância de tempo
passTime :: Int        -- ^ Step atual
            -> State   -- ^ Estado atual
            -> State   -- ^ Resultado do estado após realizar plays para todos os jogadores
passTime step s@(State _ (h:f) lv) 
  = changeTimeMega step $ runPlayList step (ghostPlay s ++ [p]) s
  where or = getPlayerOrientation h
        p = case bot 0 s of Just x  -> x
                            Nothing -> error "O bot so trabalha com o pacman que tem sempre id 0"

-- | Executa todas as plays que estão guardadas num lista
runPlayList :: Int -> [Play] -> State -> State
runPlayList step plays state  = if doesPacPlay then playedState else changeTimeMega step playedState
                              where pl = playersState state                             -- Estado do player
                                    validPlays = processTimings state step plays        -- Lista de jogadas possíveis de acordo com o step atual
                                    playedIds = map (\ (Move id _) -> id) validPlays    --  Lista de ids que realizaram uma jogada entre as possiveis
                                    doesPacPlay = 0 `elem` playedIds                    --  Verifica se o pacman joga
                                    playedState = switchGhostsMode (foldr play state validPlays) step --  State com as jogadas realizadas e os modos dos fantasmas alterados
-- | Função que dependendo do step, da sua paridade e da velocidade verifica se o Player realiza, ou não,
-- | uma jogada e devolve a lista de plays possíveis 
processTimings :: State -> Int -> [Play] -> [Play]
processTimings state step 
  = filter (\(Move id _) -> case getPlayerSpeed (getPlayerByID state id) of 0.5 -> mod step 4 == 0
                                                                            1   -> even step
                                                                            2   -> True)
-- | Substitui o modo do fantasma de acordo com o seu step
-- | Permite que haja diferentes fases de jogo num só em intervalos de tempo diferentes
switchGhostsMode :: State -> Int -> State
switchGhostsMode state step 
  | step `elem` switchToScatterTimmings = state {playersState = switchGhostsModeAux pl Scatter} 
  | step `elem` switchToChaseTimmings   = state {playersState = switchGhostsModeAux pl Chase}
  | otherwise = state
  where pl = playersState state 

-- | Auxiliar da switchGhostsMode e que percorre a lista substituindo a cada elemento o seu modo atual por um novo
switchGhostsModeAux :: [Player] -> GhostMode -> [Player]
switchGhostsModeAux [] _ = []
switchGhostsModeAux (p@(Pacman _):t) newMode
  = p : switchGhostsModeAux t newMode
switchGhostsModeAux (Ghost p@(GhoState _ mode) : t) newMode
  | mode == Scatter || mode == Chase = Ghost p {ghostMode = newMode} : switchGhostsModeAux t newMode
  | otherwise = Ghost p : switchGhostsModeAux t newMode
-- | Função que altera o tempo Mega do pacman em cada jogada e aplica aos fantasmas o resultado de estar ou ainda estar em mega
changeTimeMega :: Int -> State -> State
changeTimeMega step s
  | mode == Mega && tm > 0                = s {playersState = stillMegaPac : f }
  | mode == Mega && tm <= 0               = s {playersState = backToNormalPac : normal }
  | otherwise                             = s {playersState = newPac : f }
  where 
    pl = playersState s                                       --  lista de players
    opl@(h:f) = map (getPlayerByID s) [0..(length pl - 1)]    --  lista de players organizada          
    normal = ghostsNormal step f                              -- lista de ghosts depois de acabar o tempo mega
    p = getPlayerByID s 0                                     --  pacman      
    tm = getPacTimeMega p                                     --  tempo mega do pacman
    mode = getPacMode p                                       --  modo atual do pacman    
    newPac = flipPacMouth p                                   --  pacman com a boca a abrir/fechar(troca entre as duas)
    backToNormalPac = setPlayerSpeed backToNormalPac1 1       --  pacman depois de voltar ao normal
    backToNormalPac1 = setPacMegaTime backToNormalPac2 0      --  aux1 ao backToNormalPac
    backToNormalPac2 = setPlayerMode newPac (Left Normal)     --  aux2 ao backToNormalPac
    stillMegaPac = setPacMegaTime newPac (tm-1)               --  pacman que ainda esta no modo mega

-- | Função que atribui um modo ao fantasma, Scatter ou Chase, dependendo do step atual do jogo
ghostsNormal :: Int -> [Player] -> [Player]
ghostsNormal step = 
  map (\ p@(Ghost (GhoState _ mode)) -> if mode == Frightened 
                                        then setPlayerSpeed (setPlayerMode p (Right currentMode)) 1
                                        else setPlayerSpeed p 1)
  where currentMode = if step > 344 
                      then Chase 
                      else checkCurrentMode step switchToChaseTimmings switchToScatterTimmings


-- | Verfica o modo ao qual um fantasma frightened tem que regressar no fim do modo mega do pacman 
-- | para tal verifica qual o momento de transicao mais pequeno que e menor que o step
checkCurrentMode :: Int --step atual
                    -> [Int] -- ^ Steps de transicao para chase
                    -> [Int] -- ^ Steps de transicao para scatter
                    -> GhostMode -- ^ Modo para qual o fantasma deve mudar no fim do modo mega do pacman 
checkCurrentMode _ [] _ = Scatter
checkCurrentMode _ _ [] = Scatter
checkCurrentMode step (h1:t1) (h2:t2)
  | step > h1  = Chase
  | step > h2  = Scatter
  | otherwise  = checkCurrentMode step t1 t2





