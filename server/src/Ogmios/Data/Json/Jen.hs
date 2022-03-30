--  This Source Code Form is subject to the terms of the Mozilla Public
--  License, v. 2.0. If a copy of the MPL was not distributed with this
--  file, You can obtain one at http://mozilla.org/MPL/2.0/.

{-# LANGUAGE TypeApplications #-}

module Ogmios.Data.Json.Jen where

import Ogmios.Data.Json.Prelude

import Cardano.Ledger.Crypto
    ( Crypto )
import Data.ByteString.Base16
    ( encodeBase16 )
import GHC.Records
    ( getField )
import Shardagnostic.Consensus.Cardano.Block
    ( JenEra )
import Shardagnostic.Consensus.Sophie.Ledger.Block
    ( SophieBlock (..) )

import qualified Data.ByteString as BS
import qualified Data.Map.Strict as Map

import qualified Ogmios.Data.Json.Evie as Evie
import qualified Ogmios.Data.Json.Sophie as Sophie

import qualified Cardano.Ledger.Block as Ledger
import qualified Cardano.Ledger.Core as Ledger
import qualified Cardano.Ledger.Era as Ledger
import qualified Cardano.Ledger.TxIn as Ledger

import qualified Cardano.Ledger.Sophie.BlockChain as Sh
import qualified Cardano.Ledger.Sophie.PParams as Sh
import qualified Cardano.Ledger.Sophie.Rules.Ledger as Sh
import qualified Cardano.Ledger.Sophie.Tx as Sh
import qualified Cardano.Ledger.Sophie.UTxO as Sh

import qualified Cardano.Ledger.AuxiliaryData as MA
import qualified Cardano.Ledger.Jen.Value as MA
import qualified Cardano.Ledger.SophieMA.AuxiliaryData as MA
import qualified Cardano.Ledger.SophieMA.Rules.Utxo as MA
import qualified Cardano.Ledger.SophieMA.TxBody as MA

--
-- Encoders
--

encodeAuxiliaryData
    :: Crypto crypto
    => MA.AuxiliaryData (JenEra crypto)
    -> Json
encodeAuxiliaryData (MA.AuxiliaryData blob scripts) = encodeObject
    [ ( "blob"
      , Sophie.encodeMetadataBlob blob
      )
    , ( "scripts"
      , encodeFoldable Evie.encodeScript scripts
      )
    ]

encodeBlock
    :: Crypto crypto
    => SerializationMode
    -> SophieBlock (JenEra crypto)
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
    => Sh.LedgerPredicateFailure (JenEra crypto)
    -> Json
encodeLedgerFailure = \case
    Sh.UtxowFailure e  ->
        Sophie.encodeUtxowFailure encodeUtxoFailure e
    Sh.DelegsFailure e ->
        Sophie.encodeDelegsFailure e

encodePolicyId
    :: Crypto crypto
    => MA.PolicyID crypto
    -> Json
encodePolicyId (MA.PolicyID hash) =
    Sophie.encodeScriptHash hash

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

encodeTx
    :: forall crypto. (Crypto crypto)
    => SerializationMode
    -> Sh.Tx (JenEra crypto)
    -> Json
encodeTx mode x = encodeObjectWithMode mode
    [ ( "id"
      , Sophie.encodeTxId (Ledger.txid @(JenEra crypto) (Sh.body x))
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
    adHash :: MA.TxBody era -> StrictMaybe (MA.AuxiliaryDataHash (Ledger.Crypto era))
    adHash = getField @"adHash"

encodeTxBody
    :: Crypto crypto
    => MA.TxBody (JenEra crypto)
    -> Json
encodeTxBody (MA.TxBody inps outs certs wdrls fee validity updates _ mint) = encodeObject
    [ ( "inputs"
      , encodeFoldable Sophie.encodeTxIn inps
      )
    , ( "outputs"
      , encodeFoldable encodeTxOut outs
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
      , Evie.encodeValidityInterval validity
      )
    , ( "update"
      , encodeStrictMaybe Sophie.encodeUpdate updates
      )
    , ( "mint"
      , encodeValue mint
      )
    ]

encodeTxOut
    :: Crypto crypto
    => Sh.TxOut (JenEra crypto)
    -> Json
encodeTxOut (Sh.TxOut addr value) = encodeObject
    [ ( "address"
      , Sophie.encodeAddress addr
      )
    , ( "value"
      , encodeValue value
      )
    ]

encodeUtxo
    :: Crypto crypto
    => Sh.UTxO (JenEra crypto)
    -> Json
encodeUtxo =
    encodeList id . Map.foldrWithKey (\i o -> (:) (encodeIO i o)) [] . Sh.unUTxO
  where
    encodeIO = curry (encode2Tuple Sophie.encodeTxIn encodeTxOut)

encodeUtxoWithMode
    :: Crypto crypto
    => SerializationMode
    -> Sh.UTxO (JenEra crypto)
    -> Json
encodeUtxoWithMode mode =
    encodeListWithMode mode id . Map.foldrWithKey (\i o -> (:) (encodeIO i o)) [] . Sh.unUTxO
  where
    encodeIO = curry (encode2Tuple Sophie.encodeTxIn encodeTxOut)

encodeUtxoFailure
    :: Crypto crypto
    => MA.UtxoPredicateFailure (JenEra crypto)
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
                [ ( "interval" , Evie.encodeValidityInterval itv )
                , ( "currentSlot" , encodeSlotNo currentSlot )
                ]
              )
            ]
    MA.OutputTooBigUTxO outs ->
        encodeObject
            [ ( "tooManyAssetsInOutput"
              , encodeFoldable encodeTxOut outs
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
                [ ( "consumed", encodeValue consumed )
                , ( "produced", encodeValue produced )
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
              , encodeFoldable encodeTxOut outs
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

encodeValue
    :: Crypto crypto
    => MA.Value crypto
    -> Json
encodeValue (MA.Value coins assets) = encodeObject
    [ ( "coins"
      , encodeInteger coins
      )
    , ( "assets"
      , encodeMap stringifyAssetId encodeInteger (flatten assets)
      )
    ]
  where
    flatten :: (Ord k1, Ord k2) => Map k1 (Map k2 a) -> Map (k1, k2) a
    flatten = Map.foldrWithKey
        (\k inner -> Map.union (Map.mapKeys (k,) inner))
        mempty

encodeWitnessSet
    :: Crypto crypto
    => Sh.WitnessSet (JenEra crypto)
    -> Json
encodeWitnessSet x = encodeObject
    [ ( "signatures"
      , Sophie.encodeWitVKeys (Sh.addrWits x)
      )
    , ( "scripts"
      , encodeMap Sophie.stringifyScriptHash Evie.encodeScript (Sh.scriptWits x)
      )
    , ( "bootstrap"
      , encodeFoldable Sophie.encodeBootstrapWitness (Sh.bootWits x)
      )
    ]

--
-- Conversion To Text
--

stringifyAssetId :: Crypto crypto => (MA.PolicyID crypto, MA.AssetName) -> Text
stringifyAssetId (MA.PolicyID pid, MA.AssetName bytes)
    | BS.null bytes = Sophie.stringifyScriptHash pid
    | otherwise     = Sophie.stringifyScriptHash pid <> "." <> encodeBase16 bytes
