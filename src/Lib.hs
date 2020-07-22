{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE RecursiveDo #-}

module Lib
  ( gameMapFromList,
    gameMapNetwork,
    GameMap (..),
    GameMapEvent (..),
    Node (..),
    NodeId (..),
    Position,
  )
where

import Algebra.Graph
import Control.Monad.Fix
import Control.Monad.Writer.Strict (Writer, runWriter, tell)
import qualified Data.Foldable as F
import qualified Data.List.NonEmpty as NE
import qualified Data.Text as T
import Data.Vector ((!), (//))
import qualified Data.Vector as V
import qualified Lib.Pool as Pool
import Reflex
  ( Dynamic (..),
    DynamicWriter,
    DynamicWriterT,
    Event (..),
    EventWriter,
    EventWriterT,
    MonadHold,
    Reflex (..),
    accumDyn,
    current,
    foldDyn,
    foldDynM,
    holdDyn,
    holdUniqDyn,
    mergeList,
    runEventWriterT,
    sample,
    splitDynPure,
    tag,
    tagPromptlyDyn,
    tellEvent,
    traceDyn,
    traceEvent,
    updated,
  )
import Reflex.Class (Event (..))

newtype NodeId = NodeId Int deriving (Show, Eq)

newtype SquadId = SquadId Int deriving (Show, Eq)

type Position = (Int, Int)

data Squad = Squad
  { squadId :: SquadId,
    direction :: (NodeId, NodeId),
    speed :: Float,
    progress :: Float,
    value :: Int
  }

data Node = Node
  { --  owner :: Maybe Player,
    position :: Position,
    tick :: Int
  }
  deriving (Show)

data GameMap = GameMap
  { nodes :: V.Vector Node,
    topology :: Graph Int,
    squads :: Pool.Pool Squad
  }

data GameMapEvent
  = Tick
  | Launch NodeId
  | Arrival SquadId
  deriving (Show, Eq)

makeNode :: (Position, a) -> Node
makeNode (pos, _) = Node {position = pos, tick = 0}

-- [((10, 12), [1, 2, 3])] for example
gameMapFromList :: [(Position, [Int])] -> GameMap
gameMapFromList rawMap =
  GameMap
    { nodes = V.fromList $ makeNode <$> rawMap,
      squads = Pool.newPool,
      topology = topology_
    }
  where
    topology_ = foldr constructAndMerge empty (zip [0 ..] rawMap)
    constructAndMerge (vert, (_, connectedTo)) g =
      g + construct (fromIntegral vert) connectedTo
    construct vert =
      foldr
        ( \connectedToInd already ->
            let connectedToVert = fromIntegral connectedToInd
             in already + vert * connectedToVert
        )
        empty

gameMapNetwork ::
  (Reflex t, MonadHold t m, MonadFix m) =>
  GameMap ->
  Event t [GameMapEvent] ->
  m (Dynamic t GameMap)
gameMapNetwork gameMap events = mdo
  dGameMapWithEvents <-
    foldDyn
      updateGameMapFromEvents
      (gameMap, [])
      (mergeList [events, traceEvent "add events" additionalEvents])
  let (dynGameMap, dynEvents) = splitDynPure dGameMapWithEvents
      evEvents = current $ traceDyn "events" dynEvents
  remainEvents <- sample evEvents
  let additionalEvents = case remainEvents of
        [] -> [] <$ events
        remEvents -> updated dynEvents
  return dynGameMap

updateGameMapFromEvents :: (Foldable t) => t [GameMapEvent] -> (GameMap, [GameMapEvent]) -> (GameMap, [GameMapEvent])
updateGameMapFromEvents events (gameMap, _) = r
  where
    (updatedGameMap, feedBackEvents) = runWriter $ F.foldlM updateGameMap gameMap joinedEvents
    r = case feedBackEvents of
      [] -> (updatedGameMap, [])
      fbEvents -> updateGameMapFromEvents [feedBackEvents] (updatedGameMap, [])
    joinedEvents = concat events
--    allEvents = case fe

updateGameMap :: GameMap -> GameMapEvent -> Writer [GameMapEvent] GameMap
updateGameMap gameMap Tick = do
  uNodes <- traverse tickNode $ nodes gameMap
  uSquads <- traverse tickSquad $ squads gameMap
  return gameMap {nodes = uNodes, squads = uSquads}
updateGameMap gameMap (Launch nodeId@(NodeId id_))
  | Just (Context _ (toNode : _)) <- context (\v -> id_ == v) $ topology gameMap =
    let oldNodes = nodes gameMap
        fromNode = oldNodes ! id_
        updatedNodes = oldNodes // [(id_, fromNode {tick = 0})]
        gameMapTopology = topology gameMap
        oldSquads = squads gameMap
        newSquadConstructor squadId_ =
          Squad
            { squadId = SquadId squadId_,
              direction = (nodeId, NodeId toNode),
              speed = 0.5,
              progress = 0.0,
              value = tick fromNode
            }
        (_newSquadId, updatedSquads) = Pool.insert oldSquads newSquadConstructor
     in return gameMap {squads = updatedSquads, nodes = updatedNodes}
  | otherwise = return gameMap
updateGameMap gameMap@GameMap {nodes = nodes_, squads = squads_} (Arrival (SquadId id_))
  | Just squad <- Pool.lookup id_ squads_ =
    let squadValue = value squad
        NodeId arrivalNodeId = snd $ direction squad
        arrivalNode = nodes_ ! arrivalNodeId
        updatedSquads = Pool.delete id_ squads_
        updatedNode = arrivalNode {tick = tick arrivalNode + squadValue}
        updatedNodes = nodes_ // [(arrivalNodeId, updatedNode)]
     in do
          tell [Launch $ NodeId arrivalNodeId]
          return gameMap {squads = updatedSquads, nodes = updatedNodes}
  | otherwise = return gameMap

tickSquad :: Squad -> Writer [GameMapEvent] Squad
tickSquad squad = do
  let newProgress = min 1.0 $ progress squad + speed squad
      newSquad = squad {progress = newProgress}
  if newProgress == 1.0
    then tell [Arrival $ squadId squad]
    else pure ()
  pure newSquad

tickNode :: Node -> Writer [GameMapEvent] Node
tickNode node = pure $ node {tick = tick node + 1}
