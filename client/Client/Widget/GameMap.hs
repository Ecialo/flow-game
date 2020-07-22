module Client.Widget.GameMap where

import Client.Widget.Common (AppContext, Layer, askFont, askRenderer)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader (runReaderT)
import qualified Data.Text as T
import Data.Vector (forM_, forM)
import Formatting (int, sformat)
import Lib (GameMap (..), Node (..), NodeId, gameMapFromList)
import SDL
  ( Renderer,
    V2 (..),
    V4 (..),
    copy,
    createTextureFromSurface,
    destroyTexture,
    queryTexture,
    rendererDrawColor,
    ($=),
  )
import qualified SDL
import SDL.Font as F
import SDL.Primitive (Color, Pos, Radius, fillCircle)

renderNode :: (MonadIO m, AppContext m) => Node -> m ()
renderNode Node {position = p@(x, y), tick = tick'} = do
  sdlRenderer <- askRenderer
  fillCircle sdlRenderer (fromIntegral <$> V2 x y) 50 (V4 255 0 0 255)

  font <- askFont
  textSurf <- F.solid font (V4 0 0 255 255) (sformat int tick')
  texture <- createTextureFromSurface sdlRenderer textSurf
  ti <- queryTexture texture
  let cpos = fromIntegral <$> V2 x y
  let textureSize = V2 (SDL.textureWidth ti) (SDL.textureHeight ti)

  SDL.copy sdlRenderer texture Nothing $ Just $ SDL.Rectangle (SDL.P cpos) textureSize
  SDL.destroyTexture texture

  destroyTexture texture

renderGameMap :: (MonadIO m) => Renderer -> F.Font -> GameMap -> m ()
renderGameMap r f gameMap = runReaderT (renderGameMap' gameMap) (r, f)

renderGameMap':: (MonadIO m, AppContext m) => GameMap -> m ()
renderGameMap' GameMap {nodes = nodes_} = forM_ nodes_ renderNode