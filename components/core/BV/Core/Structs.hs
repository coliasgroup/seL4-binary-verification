module BV.Core.Structs
    ( MonadStructs (..)
    , StructsT
    , askStruct
    , mapStructsT
    , runStructsT
    , withStructs
    , withoutStructs
    ) where

import BV.Core.Types

import Control.Monad.Except (ExceptT)
import Control.Monad.Identity (Identity (runIdentity))
import Control.Monad.Reader (MonadReader (ask), ReaderT, mapReaderT, runReaderT)
import Control.Monad.RWS (RWST)
import Control.Monad.State (StateT)
import Control.Monad.Trans (MonadTrans, lift)
import Control.Monad.Trans.Maybe (MaybeT)
import GHC.Generics (Generic)
import Optics

class Monad m => MonadStructs m where
    askLookupStruct :: m (Ident -> Struct)

instance MonadStructs m => MonadStructs (ReaderT r m) where
    askLookupStruct = lift askLookupStruct

instance MonadStructs m => MonadStructs (StateT s m) where
    askLookupStruct = lift askLookupStruct

instance (Monoid w, MonadStructs m) => MonadStructs (RWST r w s m) where
    askLookupStruct = lift askLookupStruct

instance MonadStructs m => MonadStructs (MaybeT m) where
    askLookupStruct = lift askLookupStruct

instance MonadStructs m => MonadStructs (ExceptT e m) where
    askLookupStruct = lift askLookupStruct

askStruct :: MonadStructs m => Ident -> m Struct
askStruct name = ($ name) <$> askLookupStruct

-- TODO rename to NoStructsT and StructsT and export

newtype WithoutStructs a
  = WithoutStructs { unwrap :: Identity a }
  deriving newtype (Applicative, Functor, Monad)

instance MonadStructs WithoutStructs where
    askLookupStruct = return $ \name ->
        error $ "attempted to lookup struct '" ++ name.unwrap ++ "' without structs"

withoutStructs :: forall a. (forall m. MonadStructs m => m a) -> a
withoutStructs m = runIdentity (m :: WithoutStructs a).unwrap

newtype StructsT m a
  = StructsT { run :: ReaderT (Ident -> Struct) m a }
  deriving (Functor, Generic)
  deriving newtype (Applicative, Monad)

instance MonadTrans StructsT where
    lift = StructsT . lift

instance Monad m => MonadStructs (StructsT m) where
    askLookupStruct = StructsT ask

runStructsT :: (Ident -> Struct) -> StructsT m a -> m a
runStructsT f m = runReaderT m.run f

mapStructsT :: (m a -> n b) -> StructsT m a -> StructsT n b
mapStructsT f = #run %~ mapReaderT f

withStructs :: forall a. (Ident -> Struct) -> (forall m. MonadStructs m => m a) -> a
withStructs f m = runIdentity $ runStructsT f m
