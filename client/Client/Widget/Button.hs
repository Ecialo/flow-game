{-# LANGUAGE FlexibleContexts #-}

module Client.Widget.Button (button, ButtonState (..), defaultButtonCollider, defaultButtonRenderer) where

import Client.Widget.Common
  ( Layer,
    commitLayer,
    askRenderer,
    AppContext,
  )
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader (MonadReader, ask, lift)
import Foreign.C.Types
import Reflex
  ( DynamicWriter,
    Event,
    ffor,
    getPostBuild,
    holdDyn,
    holdUniqDyn,
    leftmost,
    tellDyn,
    updated,
  )
import Reflex.SDL2
  ( ReflexSDL2,
    getMouseButtonEvent,
    getMouseMotionEvent,
    mouseMotionEventPos,
  )
import SDL
  ( InputMotion (..),
    MouseButtonEventData (..),
    Point (..),
    Rectangle (..),
    Renderer,
    V2 (..),
    V4 (..),
    fillRect,
    rendererDrawColor,
    ($=),
  )
import qualified SDL.Font as F

data ButtonState
  = ButtonStateUp
  | ButtonStateOver
  | ButtonStateDown
  deriving (Eq)

type ButtonPosition = V2

type ButtonSize = V2

type ButtonCollider a = ButtonSize a -> ButtonPosition a -> Point V2 a -> Bool

type ButtonRenderer a r =
  ButtonSize a ->
  ButtonPosition a ->
  Renderer ->
  ButtonState ->
  r

defaultButtonCollider :: (Integral a) => ButtonSize a -> ButtonPosition a -> Point V2 a -> Bool
defaultButtonCollider size position (P (V2 x y)) =
  (x >= tlx && x <= brx) && (y >= tly && y <= bry)
  where
    V2 tlx tly = position
    V2 brx bry = position + size

buttonState :: Bool -> Bool -> ButtonState
buttonState isInside isDown
  | not isInside = ButtonStateUp
  | isDown = ButtonStateDown
  | otherwise = ButtonStateOver

defaultButtonRenderer ::
  (MonadIO m, Integral a) =>
  ButtonRenderer a (m ())
defaultButtonRenderer size position sdlRenderer state =
  let color = case state of
        ButtonStateUp -> V4 192 192 192 255
        ButtonStateOver -> 255
        ButtonStateDown -> V4 128 128 128 255
      cpos = fromIntegral <$> position
      csize = fromIntegral <$> size
   in do
        rendererDrawColor sdlRenderer $= color
        fillRect sdlRenderer $ Just $ Rectangle (P cpos) csize

button ::
  ( ReflexSDL2 t m,
    DynamicWriter t [Layer m] m,
    AppContext m,
    Integral a
  ) =>
  ButtonCollider a ->
  ButtonRenderer a (Layer m) ->
  ButtonSize a ->
  ButtonPosition a ->
  m (Event t ButtonState)
button collider renderer size position =
  let buttonCollider = collider size position
   in do
        evMotionData <- getMouseMotionEvent
        let evMotionPos = fmap fromIntegral . mouseMotionEventPos <$> evMotionData
            evMouseIsInside = ffor evMotionPos buttonCollider

        dMouseIsInside <- holdDyn False evMouseIsInside

        evBtn <- getMouseButtonEvent
        let evBtnIsDown = ffor evBtn $ (== Pressed) . mouseButtonEventMotion
        dButtonIsDown <- holdDyn False evBtnIsDown

        let dButtonStatePressed = buttonState <$> dMouseIsInside <*> dButtonIsDown
        evPB <- getPostBuild
        dButtonState <-
          holdDyn ButtonStateUp $
            leftmost
              [ updated dButtonStatePressed,
                ButtonStateUp <$ evPB
              ]
        sdlRenderer <- askRenderer
        commitLayer $ ffor dButtonState $ renderer size position sdlRenderer

        updated <$> holdUniqDyn dButtonState
