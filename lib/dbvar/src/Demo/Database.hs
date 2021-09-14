{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

{-# LANGUAGE GADTs #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Demo.Database where

import Prelude hiding (all)

import Data.Chain
    ( Chain
    , DeltaChain (..)
    , Edge (..)
    , chainIntoTable
    )
import Data.DBVar
import Data.Delta

import Database.Persist
    hiding ( update )
import Database.Persist.Sqlite
    hiding ( update )

import Conduit
    ( ResourceT )
import Control.Monad.Class.MonadSTM
    ( MonadSTM (..) )
import Control.Monad.IO.Class
    ( MonadIO, liftIO )
import Control.Monad.Logger
    ( NoLoggingT )
import Control.Monad.Trans.Reader
    ( ReaderT (..) )
import Data.Generics.Internal.VL
    ( Iso', iso, withIso )
import Data.Word
    ( Word32 )
import Data.Text
    ( Text )
import Data.Table
    ( DeltaDB (..)
    , Table (..)
    , tableIntoDatabase
    , Pile (..)
    )
import Database.Persist.Delta
    ( newEntityStore )
import Database.Persist.Sql
    ( SqlPersistM )
import Database.Persist.TH
    ( mkMigrate, mkPersist, mpsPrefixFields, persistLowerCase, share, sqlSettings )
import GHC.Generics
    ( Generic )
import Say
    ( say, sayShow )

import qualified Data.Chain as Chain

{-------------------------------------------------------------------------------
    Types for the database
-------------------------------------------------------------------------------}
type Address = Text
type Node = Word32

data AddressInPool = AddressInPool
    { address :: Address
    , index   :: Word32
    } deriving (Eq, Ord, Show)

share
    [ mkPersist (sqlSettings { mpsPrefixFields = False })
    , mkMigrate "migrateAll"
    ]
    [persistLowerCase|
SeqStateAddress
    seqStateAddressFrom             Node               sql=from
    seqStateAddressTo               Node               sql=to
    seqStateAddressWalletId         Word32             sql=wallet_id
    seqStateAddressAddress          Address            sql=address
    seqStateAddressIndex            Word32             sql=address_ix
    deriving Generic
|]

instance Show SeqStateAddress where
    show x =
        show (seqStateAddressTo x)
        <> " <--" <> show (seqStateAddressAddress x)
        <> "-- " <> show (seqStateAddressFrom x)

addressDBIso :: Iso' (Edge Node AddressInPool) SeqStateAddress
addressDBIso = iso
    (\Edge{from,to,via=AddressInPool{address,index}}
        -> SeqStateAddress from to 0 address index)
    (\(SeqStateAddress from to _ address index)
        -> Edge{from,to,via=AddressInPool{address,index}})

addressChainIntoTable
    :: Embedding
        (DeltaChain Node [AddressInPool])
        [DeltaDB Int SeqStateAddress]
addressChainIntoTable = 
    embedIso addressDBIso `o` (tableIntoDatabase `o` chainIntoTable Pile getPile)

embedIso :: Iso' a b -> Embedding [DeltaDB Int a] [DeltaDB Int b]
embedIso i = withIso i $ \ab ba -> mkEmbedding Embedding'
    { load = Just . fmap ba
    , write = fmap ab
    , update = \_ _ -> fmap (fmap ab)
    }

type AddressStore =
    Store SqlPersistM (DeltaChain Node [AddressInPool]) (Chain Node [AddressInPool])

newAddressStore :: SqlPersistM AddressStore
newAddressStore = embedStore addressChainIntoTable =<< newEntityStore

{-------------------------------------------------------------------------------
    Database connection
-------------------------------------------------------------------------------}
main :: IO ()
main = runSqlite ":memory:" $ do
    runMigration migrateAll

    store <- newAddressStore
    db    <- initDBVar store
        $ Chain.fromEdge Edge{from=0,to=1,via=[AddressInPool "a" 31]}

    updateDBVar db $ Chain.AppendTip 2 [AddressInPool "b" 32]
    updateDBVar db $ Chain.AppendTip 3 [AddressInPool "c" 33]
    updateDBVar db $ Chain.CollapseNode 2
    updateDBVar db $ Chain.CollapseNode 1

    (liftIO . print) =<< readDBVar db
    (liftIO . print) =<< loadS store
