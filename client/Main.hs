{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE RecursiveDo #-}

module Main where

import Client.Widget.Button (ButtonState (..), button, defaultButtonCollider, defaultButtonRenderer)
import Client.Widget.Common
  ( AppContext,
    askFont,
    askRenderer,
    commitLayer,
    commitLayers,
  )
--import qualified Client.Widget.Common as C
import Client.Widget.GameMap (renderGameMap)
import Control.Concurrent (threadDelay)
import Control.Monad (forM_, guard, void)
import Control.Monad.Reader (MonadReader (..), reader, runReaderT, withReaderT)
import Lib (gameMapNetwork, gameMapFromList, GameMapEvent(..), NodeId (..))
import Reflex
  ( Dynamic,
    DynamicWriter,
    Event,
    Performable,
    Reflex,
    delay,
    ffor,
    fmapMaybe,
    foldDyn,
    getPostBuild,
    holdDyn,
    holdUniqDyn,
    leftmost,
    performEvent_,
    runDynamicWriterT,
    runEventWriterT,
    tellDyn,
    tickLossyFromPostBuildTime,
    updated,
    zipDyn,
    zipDynWith,
    mergeList,
    splitDynPure,
  )
import Reflex.SDL2
  ( BlendMode (..),
    InputMotion (..),
    Mode (..),
    MonadIO,
    MouseButtonEventData,
    Point (..),
    Profile (..),
    Rectangle (..),
    ReflexSDL2,
    Renderer,
    V2 (..),
    V4 (..),
    WindowGraphicsContext (..),
    clear,
    createRenderer,
    createWindow,
    defaultOpenGL,
    defaultRenderer,
    defaultWindow,
    destroyRenderer,
    destroyWindow,
    fillRect,
    getAsyncEvent,
    getDeltaTickEvent,
    getKeyboardEvent,
    getMouseButtonEvent,
    getMouseMotionEvent,
    getQuitEvent,
    glCreateContext,
    glProfile,
    holdView,
    host,
    initializeAll,
    keyboardEventKeyMotion,
    keyboardEventRepeat,
    liftIO,
    mouseButtonEventMotion,
    mouseButtonEventPos,
    mouseMotionEventPos,
    mouseMotionEventState,
    performEventDelta,
    present,
    putDebugLnE,
    quit,
    rendererDrawBlendMode,
    rendererDrawColor,
    shutdownOn,
    windowGraphicsContext,
    windowHighDPI,
    windowInitialSize,
    windowResizable,
    ($=),
  )
import qualified SDL.Font as F

----------------------------------------------------------------------------

-- | An axis aligned bounding box.
data AABB = AABB InputMotion (V2 Int)

--------------------------------------------------------------------------------

-- | Convert a mouse button to an AABB.
mouseButtonToAABB :: MouseButtonEventData -> AABB
mouseButtonToAABB dat = AABB (mouseButtonEventMotion dat) pos
  where
    P pos32 = mouseButtonEventPos dat
    pos = fromIntegral <$> pos32

--------------------------------------------------------------------------------

-- | Convert a mouse button motion to color.
motionToColor :: InputMotion -> V4 Int
motionToColor Released = V4 255 0 0 128
motionToColor Pressed = V4 0 0 255 128

--------------------------------------------------------------------------------

-- | Renders an AABB using the handy SDL 2d 'Renderer'.
renderAABB :: MonadIO m => Renderer -> V4 Int -> V2 Int -> m ()
renderAABB r color pos = do
  rendererDrawColor r $= (fromIntegral <$> color)
  fillRect r $ Just $ Rectangle (P $ fromIntegral <$> pos - 10) 20

-------------------------------------------------------------------------------

-- | A type representing one layer in our app.
type Layer m = Performable m ()

----------------------------------------------------------------------

-- | Commit a layer stack that changes over time.
-- commitLayers ::
--  (ReflexSDL2 t m, DynamicWriter t [Layer m] m) =>
--  Dynamic t [Layer m] ->
--  m ()
-- commitLayers = tellDyn

----------------------------------------------------------------------

-- | Commit one layer that changes over time.
-- commitLayer ::
--  (ReflexSDL2 t m, DynamicWriter t [Layer m] m) =>
--  Dynamic t (Layer m) ->
--  m ()
-- commitLayer = tellDyn . fmap pure
ffor2 :: Reflex t => Dynamic t a -> Dynamic t b -> (a -> b -> c) -> Dynamic t c
ffor2 a b f = zipDynWith f a b

ffor2up ::
  Reflex t => Dynamic t a -> Dynamic t b1 -> ((a, b1) -> b) -> Dynamic t b
ffor2up a b = ffor (zipDyn a b)

--data ButtonState = ButtonStateUp
--                 | ButtonStateOver
--                 | ButtonStateDown
--                 deriving Eq
--

--buttonState :: Bool -> Bool -> ButtonState
--buttonState isInside isDown
--  | not isInside = ButtonStateUp
--  | isDown       = ButtonStateDown
--  | otherwise    = ButtonStateOver
--

--button
--  :: (ReflexSDL2 t m, DynamicWriter t [Layer m] m, MonadReader Renderer m)
--  => m (Event t ButtonState)
--button = do
--  evMotionData <- getMouseMotionEvent
--  let position = V2 100 100
--      size     = V2 100 100
--      V2 tlx tly = position
--      V2 brx bry = position + size
--      evMotionPos = fmap fromIntegral . mouseMotionEventPos <$> evMotionData
--      evMouseIsInside = ffor evMotionPos $ \(P (V2 x y)) ->
--        (x >= tlx && x <= brx) && (y >= tly && y <= bry)
--  dMouseIsInside <- holdDyn False evMouseIsInside
--
--  evBtn <- getMouseButtonEvent
--  let evBtnIsDown = ffor evBtn $ (== Pressed) . mouseButtonEventMotion
--  dButtonIsDown <- holdDyn False evBtnIsDown
--
--  let dButtonStatePre = buttonState <$> dMouseIsInside <*> dButtonIsDown
--  evPB         <- getPostBuild
--  dButtonState <- holdDyn ButtonStateUp $ leftmost [ updated dButtonStatePre
--                                                   , ButtonStateUp <$ evPB
--                                                   ]
--  r <- ask
--  commitLayer $ ffor dButtonState $ \st -> do
--    let color = case st of
--                  ButtonStateUp   -> V4 192 192 192 255
--                  ButtonStateOver -> 255
--                  ButtonStateDown -> V4 128 128 128 255
--    rendererDrawColor r $= color
--    fillRect r $ Just $ Rectangle (P position) size
--
--  updated <$> holdUniqDyn dButtonState

guest ::
  ( ReflexSDL2 t m,
    DynamicWriter t [Layer m] m,
    AppContext m
  ) =>
  m ()
guest = mdo
  -- Print some stuff after the network is built.
  evPB <- getPostBuild
  performEvent_ $
    ffor evPB $ \() ->
      liftIO $ putStrLn "starting up..."

  ------------------------------------------------------------------------------
  -- Get a handle on our renderer
  ------------------------------------------------------------------------------
  --  r <- ask
  ------------------------------------------------------------------------------
  -- Test async events.
  -- This will wait three seconds before coloring the background black.
  ------------------------------------------------------------------------------
  --  evDelay <- getAsyncEvent $ threadDelay 3000000
  --  dDelay <- holdDyn False $ True <$ evDelay
  --  commitLayers $
  --    ffor dDelay $ \case
  --      False -> pure $ do
  --        rendererDrawColor r $= V4 128 128 128 255
  --        fillRect r Nothing
  --      True -> pure $ do
  --        rendererDrawColor r $= V4 0 0 0 255
  --        fillRect r Nothing

  ------------------------------------------------------------------------------
  -- A button!
  ------------------------------------------------------------------------------
  let defaultButton = button defaultButtonCollider defaultButtonRenderer

  evBtnState <- defaultButton (V2 30 30) (V2 10 10)
  let evBtnPressed = fmapMaybe (guard . (== ButtonStateDown)) evBtnState
  performEvent_ $ ffor evBtnPressed $ const $ liftIO $ putStrLn "Button 1!"

--  evBtn2State <- defaultButton (V2 10 10) (V2 10 10)
--  let evBtn2Pressed = fmapMaybe (guard . (== ButtonStateDown)) evBtn2State
--  performEvent_ $ ffor evBtn2Pressed $ const $ liftIO $ putStrLn "Button 2!"

  --        performEvent_ $ renderGameMap gameMap
  ------------------------------------------------------------------------------
  -- Ghosty trail of squares
  ------------------------------------------------------------------------------
  -- Gather all mouse motion events into a list, then commit a commitLayers that
  -- renders each move as a quarter alpha'd yello or cyan square.

  --  evMouseMove <- getMouseMotionEvent
  --  dMoves      <- foldDyn (\x xs -> take 100 $ x : xs) [] evMouseMove
  --  commitLayer $ ffor dMoves $ \moves ->
  --    forM_ (reverse moves) $ \dat -> do
  --      let P pos = fromIntegral <$> mouseMotionEventPos dat
  --          color = if null (mouseMotionEventState dat)
  --                  then V4 255 255 0   128
  --                  else V4 0   255 255 128
  --      renderAABB r color pos

  ------------------------------------------------------------------------------
  -- Up and down squares
  ------------------------------------------------------------------------------
  -- Get any mouse button event and accumulate them as a list of
  -- AABBs. Commit a commitLayers of those rendered up/down AABBs.
  --  evMouseButton <- getMouseButtonEvent
  --  dBtns         <- foldDyn (\x xs -> take 100 $ x : xs) [] evMouseButton
  --  commitLayer $ ffor dBtns $ \btns ->
  --    forM_ (reverse btns) $ \dat -> do
  --      let AABB motion pos = mouseButtonToAABB dat
  --          color = motionToColor motion
  --      renderAABB r color pos

  ------------------------------------------------------------------------------
  -- An ephemeral commitLayers that only renders when a key is down, and only listens
  -- to the tick event while that key is down.
  -- This is an example of the higher-order nature of the reflex network. We
  -- can update the shape of the network in response to events within it.
  ------------------------------------------------------------------------------
  --  evKey <- getKeyboardEvent
  --  let evKeyNoRepeat = fmapMaybe (\k -> k <$ guard (not $ keyboardEventRepeat k)) evKey
  --  dPressed <- holdDyn False $ (== Pressed) . keyboardEventKeyMotion <$> evKeyNoRepeat
  --  void $ holdView (return ()) $ ffor (updated dPressed) $ \case
  --    False -> return ()
  --    True  -> do
  --      evDeltaTick <- getDeltaTickEvent
  --      dTimePressed <- foldDyn (+) 0 evDeltaTick
  --      commitLayer $ ffor dTimePressed $ \t -> do
  --        let wrap :: Float -> Int
  --            wrap x = if x > 255 then wrap (x - 255) else floor x
  --            rc    = wrap $ fromIntegral t/1000 * 255
  --            gc    = wrap $ fromIntegral t/2000 * 255
  --            bc    = wrap $ fromIntegral t/3000 * 255
  --            color :: V4 Int
  --            color = fromIntegral <$> V4 rc gc bc 255
  --        renderAABB r color 100

  ------------------------------------------------------------------------------
  -- Test our recurring timer events
  ------------------------------------------------------------------------------
  evDelta <- performEventDelta =<< tickLossyFromPostBuildTime 1
  let gameMap =
        gameMapFromList
          [ ((150, 100), [1, 2]),
            ((75, 200), [0, 3]),
            ((225, 200), [0, 3]),
            ((150, 300), [1, 2])
          ]
      tickEvent = [Tick] <$ evDelta
      launchEvent = [Launch $ NodeId 0] <$ evBtnPressed
--  dGameMapWithEvents <- gameMapNetwork gameMap $ ([Tick] <$ evDelta) <> updated newEvents
  dGameMap <- gameMapNetwork gameMap (tickEvent <> launchEvent)
--  let (dGameMap, newEvents) =  splitDynPure dGameMapWithEvents
--      update _tick = tickGameMap
--  dGameMap <- foldDyn update gameMap evDelta
--  dGameMap <- tickGameMap <$> dGameMap

  r <- askRenderer
  f <- askFont
  commitLayer $ ffor dGameMap $ renderGameMap r f

  let performDeltaSecondTimer n = do
        dTicks <- foldDyn (+) 0 $ (1 :: Int) <$ evDelta
        dDelta <- holdDyn 0 evDelta
        dElapsed <- foldDyn (+) 0 evDelta
        flip putDebugLnE id $
          updated $ do
            tickz <- dTicks
            lapse <- dElapsed
            delta <- dDelta
            return $
              unwords
                [ show n,
                  "timer -",
                  show tickz,
                  "ticks -",
                  show lapse,
                  "lapsed -",
                  show delta,
                  "delta since last tick"
                ]
  performDeltaSecondTimer 1

  ------------------------------------------------------------------------------
  -- Quit on a quit event
  ------------------------------------------------------------------------------
  evQuit <- getQuitEvent
  performEvent_ $ liftIO (putStrLn "bye!") <$ evQuit
  shutdownOn =<< delay 0 evQuit

--app :: (ReflexSDL2 t m, MonadReader Renderer m) => m ()
app :: (ReflexSDL2 t m, AppContext m) => m ()
app = do
  (_, dynLayers) <- runDynamicWriterT guest
  r <- askRenderer
  --        f <- askFont
  performEvent_ $
    ffor (updated dynLayers) $ \layers -> do
      rendererDrawColor r $= V4 0 0 0 255
      clear r
      sequence_ layers
      present r

main :: IO ()
main = do
  initializeAll
  putStrLn "loading font"
  F.initialize
  font <- F.load "assets/unispace.ttf" 18
  let ogl = defaultOpenGL {glProfile = Core Debug 3 3}
      cfg =
        defaultWindow
          { windowGraphicsContext = OpenGLContext ogl,
            windowResizable = True,
            windowHighDPI = False,
            windowInitialSize = V2 640 480
          }
  window <- createWindow "reflex-sdl2-exe" cfg
  void $ glCreateContext window

  putStrLn "creating renderer..."
  r <- createRenderer window (-1) defaultRenderer
  rendererDrawBlendMode r $= BlendAlphaBlend
  putStrLn "Start building network"
  -- Host the network with an example of how to embed your own effects.
  -- In this case it's a simple reader.
  host $ runReaderT app (r, font)
  destroyRenderer r
  destroyWindow window
  quit
