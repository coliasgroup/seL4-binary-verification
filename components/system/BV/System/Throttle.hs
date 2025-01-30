module BV.System.Throttle
    ( Priority (..)
    , ThrottleControl
    , ThrottleIn
    , Units (..)
    , controlThrottle
    , newThrottle
    , withThrottle
    ) where

import Control.Concurrent.Chan (Chan, newChan, readChan, writeChan)
import Control.Concurrent.MVar (MVar, newEmptyMVar, putMVar, takeMVar)
import Control.Exception (bracket)
import Control.Monad (forever, when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.State (evalStateT, get, gets, modify, put)
import Data.Either (fromRight)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe (fromJust, fromMaybe)
import qualified Data.Sequence as S
import Data.Void (Void)
import GHC.Generics (Generic)
import GHC.Stack (HasCallStack)
import Optics
import Optics.State.Operators ((%%=), (%=))

newThrottle :: IO (ThrottleIn, ThrottleControl)
newThrottle = do
    chan <- newChan
    return $ (ThrottleIn chan, ThrottleControl chan)

withThrottle :: ThrottleIn -> Priority -> Units -> IO a -> IO a
withThrottle throttleIn priority units m = bracket
    (do
        gate <- newEmptyMVar
        let value = Value
                { gate
                , priority
                , units
                }
        writeChan throttleIn.chan (Borrow value)
        return value)
    (\value -> do
        writeChan throttleIn.chan (Return value))
    (\value -> do
        takeMVar value.gate
        m)

controlThrottle :: ThrottleControl -> Units -> IO Void
controlThrottle throttleControl availableUnits = flip evalStateT throttleState0 . forever $ do
    msg <- liftIO $ readChan throttleControl.chan
    case msg of
        Borrow (Value { gate, priority, units }) -> do
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
    curAvailableUnits <- use #availableUnits
    maybeUnits <- undefined
    -- maybeUnits <- zoomMaybe (#byValue % singular traversed % M.le curAvailableUnits) $ do
    --     xs <- get
    --     return $ S.findIndexL snd xs <&> fst . S.index xs
    whenJust maybeUnits $ \units -> do
        #availableUnits %= (-) units
  where
    throttleState0 = ThrottleState
        { availableUnits
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

data ThrottleIn
  = ThrottleIn
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

alterWithDefault :: Ord k => a -> k -> (a -> a) -> Map k a -> Map k a
alterWithDefault def k f = M.alter (Just . f . fromMaybe def) k

removeFromSeq :: (a -> Bool) -> S.Seq a -> (a, S.Seq a)
removeFromSeq f xs =
    let i = fromJust (S.findIndexL f xs)
        a = S.index xs i
     in (a, S.deleteAt i xs)

unwrapped :: HasCallStack => Lens (Maybe a) (Maybe b) a b
unwrapped = expecting _Just

expecting :: HasCallStack => Prism s t a b -> Lens s t a b
expecting optic = partially (castOptic optic)

partially :: HasCallStack => AffineTraversal s t a b -> Lens s t a b
partially optic = withAffineTraversal optic $ \match update ->
    lens
        (fromRight (error "!isRight") . match)
        update

whenJust :: Monad m => Maybe a -> (a -> m ()) -> m ()
whenJust m f = maybe (return ()) f m
