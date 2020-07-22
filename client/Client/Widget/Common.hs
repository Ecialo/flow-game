module Client.Widget.Common where

import Reflex
  ( Dynamic,
    DynamicWriter,
    Performable,
    tellDyn,
  )
import Reflex.SDL2
  ( ReflexSDL2,
  )

import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader (MonadReader, ReaderT, reader)
import qualified SDL
import SDL (Renderer)
import SDL.Font (Font)

type Layer m = Performable m ()

type AppContext m = MonadReader (Renderer, Font) m

commitLayers ::
  (ReflexSDL2 t m, DynamicWriter t [Layer m] m, AppContext m) =>
  Dynamic t [Layer m] ->
  m ()
commitLayers = tellDyn

----------------------------------------------------------------------

-- | Commit one layer that changes over time.
commitLayer ::
  (ReflexSDL2 t m, DynamicWriter t [Layer m] m, AppContext m) =>
  Dynamic t (Layer m) ->
  m ()
commitLayer = tellDyn . fmap pure

askRenderer :: AppContext m => m Renderer
askRenderer = reader fst

askFont :: AppContext m => m Font
askFont = reader snd

--clear :: (MonadIO m, AppContext m) => m ()
--clear = do
--  r <- askRenderer
--  SDL.clear r
--
--present :: (MonadIO m, AppContext m) => m ()
--present = do
--  r <- askRenderer
--  SDL.present r