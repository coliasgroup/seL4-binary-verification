module BV.Core.Structs
    ( MonadStructs (..)
    , askStruct
    , withStructs
    , withoutStructs
    ) where

import BV.Core.Types

import Control.Monad.Except (ExceptT)
import Control.Monad.Identity (Identity (runIdentity))
import Control.Monad.Reader (MonadReader (ask), Reader, ReaderT, runReader)
import Control.Monad.RWS (RWST)
import Control.Monad.State (StateT)
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Maybe (MaybeT)

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

newtype WithoutStructs a
  = WithoutStructs { unwrap :: Identity a }
  deriving newtype (Applicative, Functor, Monad)

instance MonadStructs WithoutStructs where
    askLookupStruct = return $ \name ->
        error $ "attempted to lookup struct '" ++ name.unwrap ++ "' without structs"

withoutStructs :: forall a. (forall m. MonadStructs m => m a) -> a
withoutStructs m = runIdentity (m :: WithoutStructs a).unwrap

newtype WithStructs a
  = WithStructs { unwrap :: Reader (Ident -> Struct) a }
  deriving newtype (Applicative, Functor, Monad)

instance MonadStructs WithStructs where
    askLookupStruct = WithStructs ask

withStructs :: forall a. (Ident -> Struct) -> (forall m. MonadStructs m => m a) -> a
withStructs f m = runReader (m :: WithStructs a).unwrap f
