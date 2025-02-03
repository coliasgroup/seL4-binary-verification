module BV.System.Throttle
    ( Priority (..)
    , Throttle
    , Units (..)
    , withThrottle
    , withThrottling
    ) where

import Control.Concurrent (Chan, MVar, newChan, newEmptyMVar, putMVar, readChan,
                           takeMVar, writeChan)
import Control.Concurrent.Async (race)
import Control.Exception (bracket)
import Control.Monad (forever, unless, when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.State (evalStateT, gets, put)
import Data.Either (fromRight)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe (fromJust, fromMaybe, listToMaybe, mapMaybe)
import qualified Data.Sequence as S
import Data.Void (Void, absurd)
import GHC.Generics (Generic)
import GHC.Stack (HasCallStack)
import Optics
import Optics.State.Operators ((%%=), (%=))
import Text.Printf (printf)

withThrottling :: Units -> (Throttle -> IO a) -> IO a
withThrottling availableUnits f = do
    (throttle, throttleControl) <- newThrottle
    either absurd id <$> race (controlThrottle throttleControl availableUnits) (f throttle)

newThrottle :: IO (Throttle, ThrottleControl)
newThrottle = do
    chan <- newChan
    return (Throttle chan, ThrottleControl chan)

withThrottle :: Throttle -> Priority -> Units -> IO a -> IO a
withThrottle throttle priority units m = bracket
    (do
        gate <- newEmptyMVar
        let value = Value
                { gate
                , priority
                , units
                }
        writeChan throttle.chan (Borrow value)
        return value)
    (\value -> do
        writeChan throttle.chan (Return value))
    (\value -> do
        takeMVar value.gate
        m)

controlThrottle :: ThrottleControl -> Units -> IO Void
controlThrottle throttleControl initialUnits = flip evalStateT throttleState0 . forever $ do
    msg <- liftIO $ readChan throttleControl.chan
    case msg of
        Borrow (Value { gate, priority, units }) -> do
            unless (units >= 0) $ do
                fail "negative units"
            unless (units <= initialUnits) $ do
                fail $ printf
                    "throttle error: requested number of units (%s) exceeds total number of units (%s)"
                    (show units.unwrap)
                    (show initialUnits.unwrap)
            #byValue %=
                alterWithDefault M.empty priority
                    (alterWithDefault S.empty units
                        (S.|> (gate, False)))
        Return (Value { gate, priority, units }) -> do
            (_, open) <- #byValue % at priority % unwrapped % at units % unwrapped
                %%= removeFromSeq ((== gate) . fst)
            zoom (#byValue % at priority % unwrapped % at units) $ do
                isEmpty <- gets $ S.null . fromJust
                when isEmpty $ do
                    put Nothing
            zoom (#byValue % at priority) $ do
                isEmpty <- gets $ M.null . fromJust
                when isEmpty $ do
                    put Nothing
            when open $ do
                #availableUnits %= (+) units
    availableUnits <- use #availableUnits
    byValue <- use #byValue
    let toOpen =
            let f (priority, byUnits) = do
                    (units, gates) <- M.lookupLE availableUnits byUnits
                    i <- S.findIndexL (\(_, isOpen) -> not isOpen) gates
                    let (gate, _) = S.index gates i
                    return (priority, units, i, gate)
             in listToMaybe $ mapMaybe f (M.toList byValue)
    whenJust toOpen $ \(priority, units, i, gate) -> do
        #availableUnits %= subtract units
        assign (#byValue % at priority % _Just % at units % _Just % ix i % _2) True
        liftIO $ putMVar gate ()
  where
    throttleState0 = ThrottleState
        { availableUnits = initialUnits
        , byValue = M.empty
        }

data ThrottleControl
  = ThrottleControl
      { chan :: Chan ThrottleMessage
      }
  deriving (Eq, Generic)

data ThrottleMessage
  = Borrow Value
  | Return Value
  deriving (Eq, Generic)

data Throttle
  = Throttle
      { chan :: Chan ThrottleMessage
      }
  deriving (Eq, Generic)

data ThrottleState
  = ThrottleState
      { availableUnits :: Units
      , byValue :: Map Priority (Map Units (S.Seq (Gate, Bool)))
      }
  deriving (Eq, Generic)

data Value
  = Value
      { gate :: Gate
      , priority :: Priority
      , units :: Units
      }
  deriving (Eq, Generic)

newtype Priority
  = Priority { unwrap :: Integer }
  deriving (Eq, Generic, Ord, Show)

newtype Units
  = Units { unwrap :: Integer }
  deriving (Eq, Generic, Num, Ord, Show)

type Gate = MVar ()

whenJust :: Monad m => Maybe a -> (a -> m ()) -> m ()
whenJust m f = maybe (return ()) f m

alterWithDefault :: Ord k => a -> k -> (a -> a) -> Map k a -> Map k a
alterWithDefault def k f = M.alter (Just . f . fromMaybe def) k

removeFromSeq :: (a -> Bool) -> S.Seq a -> (a, S.Seq a)
removeFromSeq f xs =
    let i = fromJust (S.findIndexL f xs)
        a = S.index xs i
     in (a, S.deleteAt i xs)

-- TODO deduplicate with elsewhere

unwrapped :: HasCallStack => Lens (Maybe a) (Maybe b) a b
unwrapped = expecting _Just

expecting :: HasCallStack => Prism s t a b -> Lens s t a b
expecting optic = partially (castOptic optic)

partially :: HasCallStack => AffineTraversal s t a b -> Lens s t a b
partially optic = withAffineTraversal optic $ \match update ->
    lens
        (fromRight (error "!isRight") . match)
        update
