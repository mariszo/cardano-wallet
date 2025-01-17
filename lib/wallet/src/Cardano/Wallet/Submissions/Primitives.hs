{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTSyntax #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- |
Copyright: © 2022 IOHK
License: Apache-2.0

Primitive operations over the Submissions store.

These operations are guaranteed to follow the specifications individually.
For store consistence use 'Operations' module where they are composed for
that goal.

-}

module Cardano.Wallet.Submissions.Primitives
    ( Primitive (..)
    , applyPrimitive
    )
  where

import Prelude

import Cardano.Wallet.Submissions.Submissions
    ( Submissions
    , finality
    , finalityL
    , tip
    , tipL
    , transactions
    , transactionsL
    )
import Cardano.Wallet.Submissions.TxStatus
    ( HasTxId (..), TxStatus (Expired, InLedger, InSubmission) )
import Control.Lens
    ( (%~), (&), (.~) )
import Data.Foldable
    ( Foldable (..) )

import qualified Data.Map.Strict as Map

-- | Primitive operations to change a 'Submissions' store.
data Primitive slot tx where
    -- | Insert tx new transaction in the local submission store.
    AddSubmission ::
        {_expiring :: slot, _transaction :: tx} ->
        Primitive slot tx
    -- | Change a transaction state to 'InLedger'.
    MoveToLedger ::
        {_acceptance :: slot, _transaction :: tx} ->
        Primitive slot tx
    -- | Move the submission store tip slot.
    MoveTip ::
        {_tip :: slot} ->
        Primitive slot tx
    -- | Move the submission store finality slot.
    MoveFinality ::
        {_finality :: slot} ->
        Primitive slot tx
    -- | Remove a transaction from tracking in the submissions store.
    Forget ::
        {_transaction :: tx} ->
        Primitive slot tx
    deriving (Show)

-- | Apply a 'Primitive' to a submission, according to the specification.
applyPrimitive
    :: forall slot tx
    .  (Ord slot, Ord (TxId tx), HasTxId tx)
    => Primitive slot tx
    -> Submissions slot tx
    -> Submissions slot tx
applyPrimitive (AddSubmission expiring tx) s
    | expiring > tip s
      && Map.notMember (txId tx) (transactions s)
        = s & transactionsL %~ Map.insert (txId tx) (InSubmission expiring tx)
    | otherwise
        = s
applyPrimitive (MoveToLedger acceptance tx) s =
    s & transactionsL %~ Map.adjust f (txId tx)
  where
    f x@(InSubmission expiring tx')
        | acceptance > (tip s) && acceptance <= expiring =
            InLedger expiring acceptance tx'
        | otherwise = x
    f x = x
applyPrimitive (MoveTip newTip) s =
    s & (finalityL .~ if newTip <= finality s then newTip else finality s)
        . (tipL .~ newTip)
        . (transactionsL %~ fmap f)
  where
    f :: TxStatus slot tx -> TxStatus slot tx
    f status@(InLedger expiring acceptance tx)
        | acceptance > newTip = InSubmission expiring tx
        | otherwise = status
    f status@(InSubmission expiring tx)
        | expiring <= newTip = Expired expiring tx
        | otherwise = status
    f status@(Expired expiring tx)
        | expiring > newTip = InSubmission expiring tx
        | otherwise = status
    f status = status
applyPrimitive (MoveFinality newFinality) s =
    s & (finalityL .~ finality')
        . (transactionsL %~ g finality')
  where
    finality'
        | newFinality >= tip s = tip s
        | newFinality <= finality s = finality s
        | otherwise = newFinality
    g fin m = foldl' (flip $ Map.update f) m (Map.keys m)
      where
        f :: TxStatus slot tx -> Maybe (TxStatus slot tx)
        f status@(InLedger _expiring acceptance _tx)
            | acceptance <= fin = Nothing
            | otherwise = Just status
        f status@(Expired expiring _tx)
            | expiring <= fin = Nothing
            | otherwise = Just status
        f status = Just status
applyPrimitive (Forget tx) s = s & transactionsL %~ Map.delete (txId tx)
