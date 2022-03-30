--  This Source Code Form is subject to the terms of the Mozilla Public
--  License, v. 2.0. If a copy of the MPL was not distributed with this
--  file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Ogmios.Control.MonadShardagnostic
    ( MonadShardagnostic
    ) where

import qualified Control.Monad.Class.MonadAsync as Shardagnostic
import qualified Control.Monad.Class.MonadST as Shardagnostic
import qualified Control.Monad.Class.MonadThrow as Shardagnostic

-- | A type alias to ease type-signatures with Shardagnostic' internal effects.
--
-- These are required when connecting an Shardagnostic application client (more
-- exactly, required for a ChainSync client, although TxSubmission and StateQuery
-- clients both require 'MonadThrow')
type MonadShardagnostic m =
        ( Shardagnostic.MonadThrow m
        , Shardagnostic.MonadAsync m
        , Shardagnostic.MonadST m
        )
