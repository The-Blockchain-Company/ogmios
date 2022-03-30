--  This Source Code Form is subject to the terms of the Mozilla Public
--  License, v. 2.0. If a copy of the MPL was not distributed with this
--  file, You can obtain one at http://mozilla.org/MPL/2.0/.

{-# LANGUAGE TypeApplications #-}

module Ogmios.Data.Json.Evie where

import Ogmios.Data.Json.Prelude

import Cardano.Ledger.Crypto
    ( Crypto )
import GHC.Records
    ( getField )
import Shardagnostic.Consensus.Cardano.Block
    ( AllegraEra )
import Shardagnostic.Consensus.Sophie.Ledger.Block
    ( SophieBlock (..) )

import qualified Ogmios.Data.Json.Sophie as Sophie

import qualified Cardano.Ledger.AuxiliaryData as Ledger
import qualified Cardano.Ledger.Block as Ledger
import qualified Cardano.Ledger.Core as Ledger
import qualified Cardano.Ledger.Era as Ledger
import qualified Cardano.Ledger.TxIn as Ledger

import qualified Cardano.Ledger.Sophie.BlockChain as Sh
import qualified Cardano.Ledger.Sophie.PParams as Sh
import qualified Cardano.Ledger.Sophie.Rules.Ledger as Sh
import qualified Cardano.Ledger.Sophie.Tx as Sh
import qualified Cardano.Ledger.Sophie.UTxO as Sh

import qualified Cardano.Ledger.SophieMA.AuxiliaryData as MA
import qualified Cardano.Ledger.SophieMA.Rules.Utxo as MA
import qualified Cardano.Ledger.SophieMA.Timelocks as MA
import qualified Cardano.Ledger.SophieMA.TxBody as MA

--
-- Encoders
--

encodeAuxiliaryData
    :: Crypto crypto
    => MA.AuxiliaryData (AllegraEra crypto)
    -> Json
encodeAuxiliaryData (MA.AuxiliaryData blob scripts) = encodeObject
    [ ( "blob"
      , Sophie.encodeMetadataBlob blob
      )
    , ( "scripts"
      , encodeFoldable encodeScript scripts
      )
    ]

encodeBlock
    :: Crypto crypto
    => SerializationMode
    -> SophieBlock (AllegraEra crypto)
    -> Json
encodeBlock mode (SophieBlock (Ledger.Block blkHeader txs) headerHash) =
    encodeObject
    [ ( "body"
      , encodeFoldable (encodeTx mode) (Sh.txSeqTxns' txs)
      )
    , ( "header"
      , Sophie.encodeBHeader mode blkHeader
      )
    , ( "headerHash"
      , Sophie.encodeSophieHash headerHash
      )
    ]

encodeLedgerFailure
    :: Crypto crypto
    => Sh.LedgerPredicateFailure (AllegraEra crypto)
    -> Json
encodeLedgerFailure = \case
    Sh.UtxowFailure e  ->
        Sophie.encodeUtxowFailure encodeUtxoFailure e
    Sh.DelegsFailure e ->
        Sophie.encodeDelegsFailure e

encodePParams'
    :: (forall a. (a -> Json) -> Sh.HKD f a -> Json)
    -> Sh.PParams' f era
    -> Json
encodePParams' =
    Sophie.encodePParams'

encodeProposedPPUpdates
    :: Ledger.PParamsDelta era ~ Sh.PParamsUpdate era
    => Crypto (Ledger.Crypto era)
    => Sh.ProposedPPUpdates era
    -> Json
encodeProposedPPUpdates =
    Sophie.encodeProposedPPUpdates

encodeScript
    :: Crypto crypto
    => MA.Timelock crypto
    -> Json
encodeScript timelock = encodeObject
    [ ( "native", encodeTimelock timelock ) ]

encodeTimelock
    :: Crypto crypto
    => MA.Timelock crypto
    -> Json
encodeTimelock = \case
    MA.RequireSignature sig ->
        Sophie.encodeKeyHash sig
    MA.RequireAllOf xs ->
        encodeObject [( "all", encodeFoldable encodeTimelock xs )]
    MA.RequireAnyOf xs ->
        encodeObject [( "any", encodeFoldable encodeTimelock xs )]
    MA.RequireMOf n xs ->
        encodeObject [( show n, encodeFoldable encodeTimelock xs )]
    MA.RequireTimeExpire s ->
        encodeObject [( "expiresAt", encodeSlotNo s )]
    MA.RequireTimeStart s ->
        encodeObject [( "startsAt", encodeSlotNo s )]

encodeTx
    :: forall crypto. (Crypto crypto)
    => SerializationMode
    -> Sh.Tx (AllegraEra crypto)
    -> Json
encodeTx mode x = encodeObjectWithMode mode
    [ ( "id"
      , Sophie.encodeTxId (Ledger.txid @(AllegraEra crypto) (Sh.body x))
      )
    , ( "body"
      , encodeTxBody (Sh.body x)
      )
    , ( "metadata"
      , (,) <$> fmap (("hash",) . Sophie.encodeAuxiliaryDataHash) (adHash (Sh.body x))
            <*> fmap (("body",) . encodeAuxiliaryData) (Sh.auxiliaryData x)
        & encodeStrictMaybe (\(a, b) -> encodeObject [a,b])
      )
    ]
    [ ( "witness"
      , encodeWitnessSet (Sh.wits x)
      )
    ]
  where
    adHash :: MA.TxBody era -> StrictMaybe (Ledger.AuxiliaryDataHash (Ledger.Crypto era))
    adHash = getField @"adHash"

encodeTxBody
    :: Crypto crypto
    => MA.TxBody (AllegraEra crypto)
    -> Json
encodeTxBody (MA.TxBody inps outs certs wdrls fee validity updates _ _) = encodeObject
    [ ( "inputs"
      , encodeFoldable Sophie.encodeTxIn inps
      )
    , ( "outputs"
      , encodeFoldable Sophie.encodeTxOut outs
      )
    , ( "certificates"
      , encodeFoldable Sophie.encodeDCert certs
      )
    , ( "withdrawals"
      , Sophie.encodeWdrl wdrls
      )
    , ( "fee"
      , encodeCoin fee
      )
    , ( "validityInterval"
      , encodeValidityInterval validity
      )
    , ( "update"
      , encodeStrictMaybe Sophie.encodeUpdate updates
      )
    ]

encodeUtxo
    :: Crypto crypto
    => Sh.UTxO (AllegraEra crypto)
    -> Json
encodeUtxo =
    Sophie.encodeUtxo

encodeUtxoWithMode
    :: Crypto crypto
    => SerializationMode
    -> Sh.UTxO (AllegraEra crypto)
    -> Json
encodeUtxoWithMode =
    Sophie.encodeUtxoWithMode

encodeUtxoFailure
    :: Crypto crypto
    => MA.UtxoPredicateFailure (AllegraEra crypto)
    -> Json
encodeUtxoFailure = \case
    MA.BadInputsUTxO inputs ->
        encodeObject
            [ ( "badInputs"
              , encodeFoldable Sophie.encodeTxIn inputs
              )
            ]
    MA.OutsideValidityIntervalUTxO itv currentSlot ->
        encodeObject
            [ ( "outsideOfValidityInterval", encodeObject
                [ ( "interval" , encodeValidityInterval itv )
                , ( "currentSlot" , encodeSlotNo currentSlot )
                ]
              )
            ]
    MA.OutputTooBigUTxO outs ->
        encodeObject
            [ ( "tooManyAssetsInOutput"
              , encodeFoldable Sophie.encodeTxOut outs
              )
            ]
    MA.MaxTxSizeUTxO actualSize maxSize ->
        encodeObject
            [ ( "txTooLarge", encodeObject
                [ ( "maximumSize", encodeInteger maxSize )
                , ( "actualSize", encodeInteger actualSize )
                ]
              )
            ]
    MA.InputSetEmptyUTxO ->
        encodeObject
            [ ( "missingAtLeastOneInputUtxo", encodeNull )
            ]
    MA.FeeTooSmallUTxO required actual ->
        encodeObject
            [ ( "feeTooSmall", encodeObject
                [ ( "requiredFee", encodeCoin required )
                , ( "actualFee", encodeCoin actual )
                ]
              )
            ]
    MA.ValueNotConservedUTxO consumed produced ->
        encodeObject
            [ ( "valueNotConserved", encodeObject
                [ ( "consumed", encodeCoin consumed )
                , ( "produced", encodeCoin produced )
                ]
              )
            ]
    MA.WrongNetwork expected invalidAddrs ->
        encodeObject
            [ ( "networkMismatch", encodeObject
                [ ( "expectedNetwork"
                  , Sophie.encodeNetwork expected
                  )
                , ( "invalidEntities"
                  , Sophie.encodeEntities "address" Sophie.encodeAddress invalidAddrs
                  )
                ]
              )
            ]
    MA.WrongNetworkWithdrawal expected invalidAccts ->
        encodeObject
        [ ( "networkMismatch", encodeObject
            [ ( "expectedNetwork"
              , Sophie.encodeNetwork expected
              )
            , ( "invalidEntities"
              , Sophie.encodeEntities "rewardAccount" Sophie.encodeRewardAcnt invalidAccts
              )
            ]
          )
        ]
    MA.OutputTooSmallUTxO outs ->
        encodeObject
            [ ( "outputTooSmall"
              , encodeFoldable Sophie.encodeTxOut outs
              )
            ]
    MA.OutputBootAddrAttrsTooBig outs ->
        encodeObject
            [ ( "addressAttributesTooLarge"
              , encodeFoldable Sophie.encodeAddress ((\(Sh.TxOut addr _) -> addr) <$> outs)
              )
            ]
    MA.TriesToForgeBCC ->
        encodeObject
            [ ( "triesToForgeBcc", encodeNull )
            ]
    MA.UpdateFailure e ->
        Sophie.encodeUpdateFailure e

encodeValidityInterval
    :: MA.ValidityInterval
    -> Json
encodeValidityInterval x = encodeObject
    [ ( "invalidBefore"
      , encodeStrictMaybe encodeSlotNo (MA.invalidBefore x)
      )
    , ( "invalidHereafter"
      , encodeStrictMaybe encodeSlotNo (MA.invalidHereafter x)
      )
    ]

encodeWitnessSet
    :: Crypto crypto
    => Sh.WitnessSet (AllegraEra crypto)
    -> Json
encodeWitnessSet x = encodeObject
    [ ( "signatures"
      , Sophie.encodeWitVKeys (Sh.addrWits x)
      )
    , ( "scripts"
      , encodeMap Sophie.stringifyScriptHash encodeScript (Sh.scriptWits x)
      )
    , ( "bootstrap"
      , encodeFoldable Sophie.encodeBootstrapWitness (Sh.bootWits x)
      )
    ]
