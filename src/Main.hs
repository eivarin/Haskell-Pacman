module Main where

import Data.Time.Clock.POSIX
import Control.Monad.IO.Class
import UI.NCurses
import Types
import FileUtils
import Tarefa1
import Tarefa2
import Tarefa3
import Tarefa4
import Tarefa5
import Tarefa6


data Manager = Manager 
    {   
        state   :: State
    ,    pid    :: Int
    ,    step   :: Int
    ,    before :: Integer
    ,    delta  :: Integer
    ,    delay  :: Integer
    } 

-- | O sitio para mudar o mapa é na tarefa 2 
-- | alterando a variavel startingState e tambem a variavel ghostInsideHouseId
loadManager :: Manager
loadManager =  Manager startingState 0 2 0 0 defaultDelayTime 

-- | Função que muda a orientação do player dependendo da tecla que premir.
updateControlledPlayer  :: Key        -- ^ Tecla pressionada
                        -> Manager    -- ^ Manager antes da alteração
                        -> Manager    -- ^ Manager depois da alteração
updateControlledPlayer k man@( Manager st@(State _ playerlist _) id _ _ _ _ )
  | k == KeyUpArrow    = man { state =  st {playersState = changeOrientation U l}}
  | k == KeyDownArrow  = man { state =  st {playersState = changeOrientation D l}}
  | k == KeyLeftArrow  = man { state =  st {playersState = changeOrientation L l}}
  | k == KeyRightArrow = man { state =  st {playersState = changeOrientation R l}}
  | otherwise          = man
  where l = selectPlayer id playerlist

-- *Funções de dar print à interface no ecrã

-- | Função que atualiza o ecrã com a a interface do jogo.
-- | PS: esta função foi alterada para poder integrar as cores do Ncurses
updateScreen  :: Window     -- ^ Janela que contem o ecrã que vai ser atualizado
              -> ColorID    -- ^ ID da cor usada por defeito quando não existe cor para uma string
              -> Manager    -- ^ Manager com a informação a usar na interface
              -> Curses ()  -- ^ Resultado do Curses
updateScreen w a man@( Manager st@(State m playerlist _) id _ _ _ _ ) =
                  do
                    coords <- drawColorInstructions w cis (0,0)
                    drawPlayerStats (length cis) st a w
                    render
                  where cis = fromMazeToColorInstruct $ placePlayersOnMap playerlist m

-- | Funcao que escreve os player stats no ecra. Parte por baixo do Maze.
drawPlayerStats  :: Int           -- ^ Tamanho do maze (Variavel usada para saber onde começar as infos)
                    -> State      -- ^ State com a informção a ser escrita no ecrã
                    -> ColorID    -- ^ ID da cor usada por defeito quando não existe cor para uma string
                    -> Window     -- ^ Janela que contem o ecrã que vai ser atualizado
                    -> Curses ()  -- ^ Resultado do Curses
drawPlayerStats size s clr w  = do
                              updateWindow w $ do
                                setColor clr
                                moveCursor (toInteger size) (toInteger 0) 
                                drawString (instStateShowNew s)
                              render

-- | Função responsável por desenhar o labirinto com cores.
-- | Percorre uma lista de ColorInstructions pregerada para desenhar cada string com a cor ddesejada.
drawColorInstructions :: Window             -- ^ Janela que contem o ecrã que vai ser atualizado
                      -> ColorInstructions  -- ^ Instruções com as cores e as respetivas string a serem imprimidas no ecrã
                      -> Coords             -- ^ Acumulador de coordenadas
                      -> Curses Coords      -- ^ Resultado do Curses (acumulador de coordanadas)
drawColorInstructions w [] coords = return coords
drawColorInstructions w (h:t) (y,x) = do coords <- drawColorInstruct w h (y,x)
                                         drawColorInstructions w t coords

-- | Função responsável por desenhar uma unica linha do labirinto com cores.
drawColorInstruct :: Window           -- ^ Janela que contem o ecrã que vai ser atualizado
                  -> ColorInstruction -- ^ Instrução unica as cores e as respetivas string correspondentes a uma linha a serem imprimidas no ecrã
                  -> Coords           -- ^ Acumulador de coordenadas
                  -> Curses Coords    -- ^ Resultado do Curses (acumulador de coordanadas)
drawColorInstruct w [] (y,x) = do return (y+1,0)
drawColorInstruct w ci@((st,clr):t) (y,x)
  = do
      color_schema <- coloredCursesString clr
      updateWindow w $ do
        setColor color_schema
        moveCursor (toInteger y) (toInteger x) 
        drawString st
      render
      drawColorInstruct w t (y,x+length st)

-- | Função usada quando o pacman perde as vidas todas. Corresponde a um ecrã de "perdeste".
-- | Sustitui a função updateScreen a partir do momento que o pacman morre
uLostScreen :: Window     -- ^ Janela que contem o ecrã que vai ser atualizado
              -> ColorID    -- ^ ID da cor usada por defeito quando não existe cor para uma string
              -> Manager    -- ^ Manager com a informação a usar na interface
              -> Curses ()  -- ^ Resultado do Curses
uLostScreen w a man =
                  do
                    updateWindow w $ do
                      clear
                      setColor a
                      moveCursor 0 0 
                      drawString $ "\n" ++ "\n" ++ "\n" ++ "\n" ++ "            Perdeste" 
                                ++ "\n" ++ "\n" ++ "\n" ++ "\n" ++ "  Melhor Sorte Para a Proxima" 
                                ++ "\n" ++ "\n" ++ "\n" ++ "\n" ++ "            Pontuacao:" ++ show (getPlayerPoints (getPlayerByID (state man) 0)) 
                    render

-- *Funções de gestão de tempo do jogo

-- | Função que devolve o tempo atual em integer
currentTime :: IO Integer
currentTime = fmap ( round . (* 1000) ) getPOSIXTime

-- | Função que faz update ao delta atual do manager quando o delta não é maior que o delay
updateTime  :: Integer    -- ^ Tempo atual 
            -> Manager    -- ^ Manager antes da alteração
            -> Manager    -- ^ Manager depois da alteração
updateTime now man@( Manager _ _ _ before _ _ ) = man {delta = now - before}

-- | Função que reinicia o contador do delta a zero 
resetTimer :: Integer    -- ^ Tempo atual 
            -> Manager    -- ^ Manager antes da alteração
            -> Manager    -- ^ Manager depois da alteração
resetTimer now man = man {before = now, delta = 0}

-- | Função que chama a passTime para as jogadas serem executadas e para o tempo passar no state
-- | Tambem chama a resetTimer para o contador do delta voltar a zero 
nextFrame :: Integer    -- ^ Tempo atual 
          -> Manager    -- ^ Manager antes da alteração
          -> Manager    -- ^ Manager depois da alteração
nextFrame now man@( Manager st _ stp _ _ _ ) 
  = resetTimer now (man {step = stp + 1, state = passTime stp st})

-- * Funções principais

-- | Loop principal do jogo. 
-- | Função que está constantemente a ser invocada para o jogo se manter aberto.
loop  :: Window     -- ^ Window do jogo
      -> Manager    -- ^ Manager com a informação toda do jogo
      -> Curses ()  -- ^ Resultado do Curses
loop w man@(Manager s pid step bf delt del ) = do 
  color_schema <- newColorID ColorRed ColorBlack  255
  now <- liftIO  currentTime
  if getPacMode (getPlayerByID s 0) /= Dying 
    then do
    updateScreen w color_schema man
    if delt > del
      then loop w $ nextFrame now man
      else do
            ev <- getEvent w $ Just 0
            case ev of
                  Nothing -> loop w (updateTime now man)
                  Just (EventSpecialKey arrow ) -> loop w $ updateControlledPlayer arrow (updateTime now man)
                  Just ev' ->
                    if ev' == EventCharacter 'q'
                      then return ()
                      else loop w (updateTime now man)
    else do uLostScreen w color_schema man
            ev <- getEvent w $ Just 0
            case ev of
                  Nothing -> loop w (updateTime now man)
                  Just ev' ->
                    if ev' == EventCharacter 'q'
                      then return ()
                      else loop w (updateTime now man)

main :: IO ()
main =
  runCurses $ do
    setEcho False
    setCursorMode CursorInvisible
    w <- defaultWindow
    updateWindow w $ do clear
    loop w loadManager