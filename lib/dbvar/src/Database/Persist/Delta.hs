{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
module Database.Persist.Delta (
    -- * Synopsis
    -- | Manipulating SQL database tables using delta encodings
    -- via the "persistent" package.
    
    -- * Store
      DBIO, newEntityStore --, newSqlStore
    ) where

import Prelude hiding (all)

import Conduit
    ( ResourceT )
import Control.Monad
    ( forM, void )
import Control.Monad.Class.MonadSTM
    ( MonadSTM (..) )
import Control.Monad.IO.Class
    ( MonadIO, liftIO )
import Control.Monad.Logger
    ( NoLoggingT )
import Control.Monad.Trans.Reader
    ( ReaderT (..) )
import Data.DBVar
    ( Store (..) )
import Data.Delta
    ( Delta (..) )
import Data.Table
    ( Table (..), DeltaDB (..), Pile (..) )
import Database.Persist
    ( Filter, PersistRecordBackend, ToBackendKey, Key, Entity, selectList )
import Database.Persist.Sql
    ( fromSqlKey, toSqlKey, SqlBackend )
import Database.Schema
    ( IsRow, (:.), Col (..), Primary (..) )
import Say
    ( say, sayShow )

-- FIXME: Replace with IOSim stuff later.
import Data.IORef
    ( IORef, newIORef, readIORef, writeIORef )

import qualified Database.Schema as DB
import qualified Data.Table as Table
import qualified Database.Persist as Persist

{-------------------------------------------------------------------------------
    Database operations
-------------------------------------------------------------------------------}
-- | Helper abstraction for a Database backend
data Database m key row = Database
    { selectAll   :: m [(key, row)]
    , deleteAll   :: m ()
    , insertMany  :: [row] -> m [key]
    , repsertMany :: [(key, row)] -> m ()
    , delete1     :: key -> m ()
    , replace1    :: (key, row) -> m ()
    }

-- | Database monad required by the "persistent" package.
type DBIO = ReaderT SqlBackend (NoLoggingT (ResourceT IO))
instance MonadSTM (NoLoggingT (ResourceT IO)) where
    type instance STM (NoLoggingT (ResourceT IO)) = STM IO
    atomically = liftIO . atomically

-- | Database table for 'Entity'.
persistDB
    :: forall row. ( PersistRecordBackend row SqlBackend
    , ToBackendKey SqlBackend row, Show row )
    => Database DBIO Int row
persistDB = Database
    { selectAll = map toPair <$> Persist.selectList all []
    , deleteAll = Persist.deleteWhere all
    , insertMany = fmap (map fromKey) . Persist.insertMany
    , repsertMany = Persist.repsertMany . map (\(key,val) -> (toKey key, val))
    , delete1 = Persist.delete . toKey
    , replace1 = \(key,val) -> Persist.replace (toKey key) val
    }
  where
    all = [] :: [Filter row]

    toPair (Persist.Entity key val) = (fromKey key, val)

    fromKey = fromIntegral . fromSqlKey
    toKey :: Int -> Key row
    toKey = toSqlKey . fromIntegral

-- | SQL database backend
sqlDB
    :: (MonadIO m, IsRow (row :. Col "id" Primary))
    => Database m Int row
sqlDB = undefined

{-------------------------------------------------------------------------------
    Database operations
-------------------------------------------------------------------------------}
-- | Construct a 'Store' from an SQL table.
newSqlStore
    :: (MonadIO m, IsRow (row :. Col "id" Primary), Show row)
    => m (Store DBIO [DeltaDB Int row] (Table row))
newSqlStore = newDatabaseStore sqlDB

-- | Construct a 'Store' for 'Entity'.
--
-- FIXME: This function should also do \"migrations\", i.e.
-- create the database table in the first place.
newEntityStore
    :: forall row m.
    ( PersistRecordBackend row SqlBackend
    , ToBackendKey SqlBackend row, Show row
    , MonadIO m )
    => m (Store DBIO [DeltaDB Int row] (Table row))
newEntityStore = newDatabaseStore persistDB

-- | Helper function to create a 'Store' using a 'Database' backend.
newDatabaseStore
    :: forall m n row. (MonadIO m, MonadIO n, Show row)
    => Database m Int row
    -> n (Store m [DeltaDB Int row] (Table row))
newDatabaseStore db = do
    ref <- liftIO $ newIORef Nothing
    let rememberSupply table = liftIO $ writeIORef ref $ Just $ uids table
    pure $ Store
        { loadS   = do
            debug $ do
                say "\n** loadS"
                liftIO . print =<< selectAll db
            -- read database table, preserve keys
            table <- Table.fromRows <$> selectAll db
            -- but use our own unique ID supply
            liftIO (readIORef ref) >>= \case
                Just supply  -> pure $ Just table{uids = supply}
                Nothing      -> do
                    rememberSupply table
                    pure $ Just table
        , writeS  = \table -> void $ do
            deleteAll db -- delete any old data in the table first
            _ <- insertMany db $ getPile $ Table.toPile table
            rememberSupply table
        , updateS = \table ds -> do
            debug $ do
                say "\n** updateS table deltas"
                sayShow $ table
                sayShow $ ds
            mapM_ (update1 table) ds
            rememberSupply (apply ds table) -- need to use updated supply
        }
  where
    debug m = if False then m else pure ()

    update1 _ (InsertManyDB zs) = void $ repsertMany db zs
    update1 _ (DeleteManyDB ks) = void $ forM ks $ delete1 db
    update1 _ (UpdateManyDB zs) = void $ forM zs $ replace1 db

{- Note [Unique ID supply in newDBStore]

We expect that updating the store and loading the value
is the same as first loading the value and then apply the delta,
i.e. we expect that the two actions

    loadS >>= \a -> updateS a da >>= loadS
    loadS >>= \a -> pure $ apply da a

are operationally equivalent.
However, this is only the case if we keep track of the supply
of unique IDs for the table! Otherwise, loading the table
from the database again can mess up the supply.
-}
-- FIXME: For clarity, we may want to implement this in terms
-- of a product of stores ("semidirect product").
