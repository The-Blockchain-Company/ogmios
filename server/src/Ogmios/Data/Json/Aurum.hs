--  This Source Code Form is subject to the terms of the Mozilla Public
--  License, v. 2.0. If a copy of the MPL was not distributed with this
--  file, You can obtain one at http://mozilla.org/MPL/2.0/.

{-# LANGUAGE TypeApplications #-}

module Ogmios.Data.Json.Aurum where

import Ogmios.Data.Json.Prelude

import Cardano.Ledger.Crypto
    ( Crypto )
import Data.ByteString.Base16
    ( encodeBase16 )
import Data.MemoBytes
    ( memobytes )
import GHC.Records
    ( getField )
import Shardagnostic.Consensus.Cardano.Block
    ( AurumEra )
import Shardagnostic.Consensus.Sophie.Ledger.Block
    ( SophieBlock (..) )
import Prettyprinter
    ( pretty )

import qualified Data.Map.Strict as Map

import qualified Ogmios.Data.Json.Evie as Evie
import qualified Ogmios.Data.Json.Jen as Jen
import qualified Ogmios.Data.Json.Sophie as Sophie

import qualified Cardano.Crypto.Hash.Class as CC

import qualified Cardano.Ledger.Block as Ledger
import qualified Cardano.Ledger.Era as Ledger
import qualified Cardano.Ledger.SafeHash as Ledger
import qualified Cardano.Ledger.TxIn as Ledger

import qualified Cardano.Ledger.Sophie.API as Sh
import qualified Cardano.Ledger.Sophie.PParams as Sh
import qualified Cardano.Ledger.Sophie.Rules.Ledger as Sh

import qualified Cardano.Ledger.Aurum.Data as Al
import qualified Cardano.Ledger.Aurum.Genesis as Al
import qualified Cardano.Ledger.Aurum.Language as Al
import qualified Cardano.Ledger.Aurum.PlutusScriptApi as Al
import qualified Cardano.Ledger.Aurum.PParams as Al
import qualified Cardano.Ledger.Aurum.Rules.Utxo as Al
import qualified Cardano.Ledger.Aurum.Rules.Utxos as Al
import qualified Cardano.Ledger.Aurum.Rules.Utxow as Al
import qualified Cardano.Ledger.Aurum.Scripts as Al
import qualified Cardano.Ledger.Aurum.Tx as Al
import qualified Cardano.Ledger.Aurum.TxBody as Al
import qualified Cardano.Ledger.Aurum.TxInfo as Al
import qualified Cardano.Ledger.Aurum.TxSeq as Al
import qualified Cardano.Ledger.Aurum.TxWitness as Al

import qualified Cardano.Ledger.Aurum.Tools as Al.Tools

--
-- Encoders
--

encodeAurumPredFail
    :: Crypto crypto
    => Al.AurumPredFail (AurumEra crypto)
    -> Json
encodeAurumPredFail = \case
    Al.MissingRedeemers missing ->
        encodeObject
            [ ( "missingRequiredRedeemers", encodeObject
                [ ( "missing", encodeFoldable encodeMissingRedeemer missing)
                ]
              )
            ]
    Al.MissingRequiredDatums missing provided ->
        encodeObject
            [ ( "missingRequiredDatums", encodeObject
                [ ( "provided", encodeFoldable encodeDataHash provided )
                , ( "missing", encodeFoldable encodeDataHash missing )
                ]
              )
            ]
    Al.NonOutputSupplimentaryDatums unallowed acceptable ->
        encodeObject
            [ ( "unspendableDatums", encodeObject
                [ ( "nonSpendable", encodeFoldable encodeDataHash unallowed )
                , ( "acceptable", encodeFoldable encodeDataHash acceptable )
                ]
              )
            ]
    Al.PPViewHashesDontMatch provided inferred ->
        encodeObject
            [ ( "extraDataMismatch", encodeObject
                [ ( "provided", encodeStrictMaybe encodeScriptIntegrityHash provided )
                , ( "inferredFromParameters", encodeStrictMaybe encodeScriptIntegrityHash inferred )
                ]
              )
            ]
    Al.MissingRequiredSigners keys ->
        encodeObject
            [ ( "missingRequiredSignatures"
              , encodeFoldable Sophie.encodeKeyHash keys
              )
            ]
    Al.UnspendableUTxONoDatumHash utxos ->
        encodeObject
            [ ( "unspendableScriptInputs"
              , encodeFoldable Sophie.encodeTxIn utxos
              )
            ]
    Al.ExtraRedeemers redeemers ->
        encodeObject
            [ ( "extraRedeemers"
              , encodeFoldable (encodeText . stringifyRdmrPtr) redeemers
              )
            ]
    Al.WrappedSophieEraFailure e ->
        Sophie.encodeUtxowFailure  encodeUtxoFailure e

encodeAuxiliaryData
    :: Crypto crypto
    => Al.AuxiliaryData (AurumEra crypto)
    -> Json
encodeAuxiliaryData (Al.AuxiliaryData blob scripts) = encodeObject
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
    -> SophieBlock (AurumEra crypto)
    -> Json
encodeBlock mode (SophieBlock (Ledger.Block blkHeader txs) headerHash) =
    encodeObject
    [ ( "body"
      , encodeFoldable (encodeTx mode) (Al.txSeqTxns txs)
      )
    , ( "header"
      , Sophie.encodeBHeader mode blkHeader
      )
    , ( "headerHash"
      , Sophie.encodeSophieHash headerHash
      )
    ]

encodeCollectError
    :: Crypto crypto
    => Al.CollectError crypto
    -> Json
encodeCollectError = \case
    Al.NoRedeemer purpose ->
        encodeObject [ ( "noRedeemer", encodeScriptPurpose purpose ) ]
    Al.NoWitness hash ->
        encodeObject [ ( "noWitness", Sophie.encodeScriptHash hash ) ]
    Al.NoCostModel lang ->
        encodeObject [ ( "noCostModel", encodeLanguage lang ) ]

encodeCostModel
    :: Al.CostModel
    -> Json
encodeCostModel (Al.CostModel model) =
    encodeMap id encodeInteger model

encodeData
    :: Al.Data era
    -> Json
encodeData (Al.DataConstr datum) =
    encodeShortByteString encodeByteStringBase64 (memobytes datum)

encodeDataHash
    :: Crypto crypto
    => Al.DataHash crypto
    -> Json
encodeDataHash =
    Sophie.encodeHash . Ledger.extractHash

encodeExUnits
    :: Al.ExUnits
    -> Json
encodeExUnits units =  encodeObject
    [ ( "memory", encodeNatural (Al.exUnitsMem units) )
    , ( "steps", encodeNatural (Al.exUnitsSteps units) )
    ]

encodeGenesis
    :: Al.AurumGenesis
    -> Json
encodeGenesis x = encodeObject
    [ ( "coinsPerUtxoWord"
      , encodeCoin (Al.coinsPerUTxOWord x)
      )
    , ( "costModels"
      , encodeMap stringifyLanguage encodeCostModel (Al.costmdls x)
      )
    , ( "prices"
      , encodePrices (Al.prices x)
      )
    , ( "maxExecutionUnitsPerTransaction"
      , encodeExUnits (Al.maxTxExUnits x)
      )
    , ( "maxExecutionUnitsPerBlock"
      , encodeExUnits (Al.maxBlockExUnits x)
      )
    , ( "maxValueSize"
      , encodeNatural (Al.maxValSize x)
      )
    , ( "collateralPercentage"
      , encodeNatural (Al.collateralPercentage x)
      )
    , ( "maxCollateralInputs"
      , encodeNatural (Al.maxCollateralInputs x)
      )
    ]

encodeLanguage
    :: Al.Language
    -> Json
encodeLanguage =
    encodeText . stringifyLanguage

encodeLedgerFailure
    :: Crypto crypto
    => Sh.LedgerPredicateFailure (AurumEra crypto)
    -> Json
encodeLedgerFailure = \case
    Sh.UtxowFailure e ->
        encodeAurumPredFail e
    Sh.DelegsFailure e ->
        Sophie.encodeDelegsFailure e

encodeMissingRedeemer
    :: Crypto crypto
    => (Al.ScriptPurpose crypto, Sh.ScriptHash crypto)
    -> Json
encodeMissingRedeemer (purpose, hash) =
    encodeObject
        [ ( Sophie.stringifyScriptHash hash
          , encodeScriptPurpose purpose
          )
        ]

encodePParams'
    :: (forall a. (a -> Json) -> Sh.HKD f a -> Json)
    -> Al.PParams' f era
    -> Json
encodePParams' encodeF x = encodeObject
    [ ( "minFeeCoefficient"
      , encodeF encodeNatural (Al._minfeeA x)
      )
    , ( "minFeeConstant"
      , encodeF encodeNatural (Al._minfeeB x)
      )
    , ( "maxBlockBodySize"
      , encodeF encodeNatural (Al._maxBBSize x)
      )
    , ( "maxBlockHeaderSize"
      , encodeF encodeNatural (Al._maxBHSize x)
      )
    , ( "maxTxSize"
      , encodeF encodeNatural (Al._maxTxSize x)
      )
    , ( "stakeKeyDeposit"
      , encodeF encodeCoin (Al._keyDeposit x)
      )
    , ( "poolDeposit"
      , encodeF encodeCoin (Al._poolDeposit x)
      )
    , ( "poolRetirementEpochBound"
      , encodeF encodeEpochNo (Al._eMax x)
      )
    , ( "desiredNumberOfPools"
      , encodeF encodeNatural (Al._nOpt x)
      )
    , ( "poolInfluence"
      , encodeF encodeNonNegativeInterval (Al._a0 x)
      )
    , ( "monetaryExpansion"
      , encodeF encodeUnitInterval (Al._rho x)
      )
    , ( "treasuryExpansion"
      , encodeF encodeUnitInterval (Al._tau x)
      )
    , ( "decentralizationParameter"
      , encodeF encodeUnitInterval (Al._d x)
      )
    , ( "extraEntropy"
      , encodeF Sophie.encodeNonce (Al._extraEntropy x)
      )
    , ( "protocolVersion"
      , encodeF Sophie.encodeProtVer (Al._protocolVersion x)
      )
    , ( "minPoolCost"
      , encodeF encodeCoin (Al._minPoolCost x)
      )
    , ( "coinsPerUtxoWord"
      , encodeF encodeCoin (Al._coinsPerUTxOWord x)
      )
    , ( "costModels"
      , encodeF (encodeMap stringifyLanguage encodeCostModel) (Al._costmdls x)
      )
    , ( "prices"
      , encodeF encodePrices (Al._prices x)
      )
    , ( "maxExecutionUnitsPerTransaction"
      , encodeF encodeExUnits (Al._maxTxExUnits x)
      )
    , ( "maxExecutionUnitsPerBlock"
      , encodeF encodeExUnits (Al._maxBlockExUnits x)
      )
    , ( "maxValueSize"
      , encodeF encodeNatural (Al._maxValSize x)
      )
    , ( "collateralPercentage"
      , encodeF encodeNatural (Al._collateralPercentage x)
      )
    , ( "maxCollateralInputs"
      , encodeF encodeNatural (Al._maxCollateralInputs x)
      )
    ]

encodePrices
    :: Al.Prices
    -> Json
encodePrices prices =  encodeObject
    [ ( "memory", encodeNonNegativeInterval (Al.prMem prices) )
    , ( "steps", encodeNonNegativeInterval (Al.prSteps prices) )
    ]

encodeProposedPPUpdates
    :: Crypto crypto
    => Sh.ProposedPPUpdates (AurumEra crypto)
    -> Json
encodeProposedPPUpdates (Sh.ProposedPPUpdates m) =
    encodeMap Sophie.stringifyKeyHash (encodePParams' encodeStrictMaybe) m

encodeRedeemers
    :: Crypto crypto
    => Al.Redeemers (AurumEra crypto)
    -> Json
encodeRedeemers (Al.Redeemers redeemers) =
    encodeMap stringifyRdmrPtr encodeDataAndUnits redeemers
  where
    encodeDataAndUnits
        :: (Al.Data era, Al.ExUnits)
        -> Json
    encodeDataAndUnits (redeemer, units) = encodeObject
        [ ( "redeemer", encodeData redeemer )
        , ( "executionUnits", encodeExUnits units )
        ]

encodeScript
    :: Crypto crypto
    => Al.Script (AurumEra crypto)
    -> Json
encodeScript = \case
    Al.TimelockScript nativeScript -> encodeObject
        [ ( "native"
          , Evie.encodeTimelock nativeScript
          )
        ]
    Al.PlutusScript lang serializedScript -> encodeObject
        [ ( stringifyLanguage lang
          , encodeShortByteString encodeByteStringBase64 serializedScript
          )
        ]

encodeScriptPurpose
    :: Crypto crypto
    => Al.ScriptPurpose crypto
    -> Json
encodeScriptPurpose = \case
    Al.Spending txIn ->
        encodeObject [ ( "spend", Sophie.encodeTxIn txIn ) ]
    Al.Minting policyId ->
        encodeObject [ ( "mint", Jen.encodePolicyId policyId ) ]
    Al.Certifying cert ->
        encodeObject [ ( "certificate", Sophie.encodeDCert cert ) ]
    Al.Rewarding acct ->
        encodeObject [ ( "withdrawal", Sophie.encodeRewardAcnt acct ) ]

encodeTx
    :: forall crypto. Crypto crypto
    => SerializationMode
    -> Al.ValidatedTx (AurumEra crypto)
    -> Json
encodeTx mode x = encodeObjectWithMode mode
    [ ( "id"
      , Sophie.encodeTxId (Ledger.txid @(AurumEra crypto) (Al.body x))
      )
    , ( "body"
      , encodeTxBody (Al.body x)
      )
    , ( "metadata"
      , (,) <$> fmap (("hash",) . Sophie.encodeAuxiliaryDataHash) (adHash (Al.body x))
            <*> fmap (("body",) . encodeAuxiliaryData) (Al.auxiliaryData x)
        & encodeStrictMaybe (\(a, b) -> encodeObject [a,b])
      )
    ]
    [ ( "witness"
      , encodeWitnessSet (Al.wits x)
      )
    ]
  where
    adHash :: Al.TxBody era -> StrictMaybe (Al.AuxiliaryDataHash (Ledger.Crypto era))
    adHash = getField @"adHash"

encodeTxBody
    :: Crypto crypto
    => Al.TxBody (AurumEra crypto)
    -> Json
encodeTxBody x = encodeObject
    [ ( "inputs"
      , encodeFoldable Sophie.encodeTxIn (Al.inputs x)
      )
    , ( "collaterals"
      , encodeFoldable Sophie.encodeTxIn (Al.collateral x)
      )
    , ( "outputs"
      , encodeFoldable encodeTxOut (Al.outputs x)
      )
    , ( "certificates"
      , encodeFoldable Sophie.encodeDCert (Al.txcerts x)
      )
    , ( "withdrawals"
      , Sophie.encodeWdrl (Al.txwdrls x)
      )
    , ( "fee"
      , encodeCoin (Al.txfee x)
      )
    , ( "validityInterval"
      , Evie.encodeValidityInterval (Al.txvldt x)
      )
    , ( "update"
      , encodeStrictMaybe encodeUpdate (Al.txUpdates x)
      )
    , ( "mint"
      , Jen.encodeValue (Al.mint x)
      )
    , ( "network"
      , encodeStrictMaybe Sophie.encodeNetwork (Al.txnetworkid x)
      )
    , ( "scriptIntegrityHash"
      , encodeStrictMaybe encodeScriptIntegrityHash (Al.scriptIntegrityHash x)
      )
    , ( "requiredExtraSignatures"
      , encodeFoldable Sophie.encodeKeyHash (Al.reqSignerHashes x)
      )
    ]

encodeTxOut
    :: Crypto crypto
    => Al.TxOut (AurumEra crypto)
    -> Json
encodeTxOut (Al.TxOut addr value datum) = encodeObject
    [ ( "address"
      , Sophie.encodeAddress addr
      )
    , ( "value"
      , Jen.encodeValue value
      )
    , ( "datum"
      , encodeStrictMaybe encodeDataHash datum
      )
    ]

encodeUpdate
    :: Crypto crypto
    => Sh.Update (AurumEra crypto)
    -> Json
encodeUpdate (Sh.Update update epoch) = encodeObject
    [ ( "proposal"
      , encodeProposedPPUpdates update
      )
    , ( "epoch"
      , encodeEpochNo epoch
      )
    ]

encodeUtxo
    :: Crypto crypto
    => Sh.UTxO (AurumEra crypto)
    -> Json
encodeUtxo =
    encodeList id . Map.foldrWithKey (\i o -> (:) (encodeIO i o)) [] . Sh.unUTxO
  where
    encodeIO = curry (encode2Tuple Sophie.encodeTxIn encodeTxOut)

encodeUtxoWithMode
    :: Crypto crypto
    => SerializationMode
    -> Sh.UTxO (AurumEra crypto)
    -> Json
encodeUtxoWithMode mode =
    encodeListWithMode mode id . Map.foldrWithKey (\i o -> (:) (encodeIO i o)) [] . Sh.unUTxO
  where
    encodeIO = curry (encode2Tuple Sophie.encodeTxIn encodeTxOut)

encodeUtxoFailure
    :: forall crypto. Crypto crypto
    => Al.UtxoPredicateFailure (AurumEra crypto)
    -> Json
encodeUtxoFailure = \case
    Al.BadInputsUTxO inputs ->
        encodeObject
            [ ( "badInputs"
              , encodeFoldable Sophie.encodeTxIn inputs
              )
            ]
    Al.OutsideValidityIntervalUTxO itv currentSlot ->
        encodeObject
            [ ( "outsideOfValidityInterval", encodeObject
                [ ( "interval" , Evie.encodeValidityInterval itv )
                , ( "currentSlot" , encodeSlotNo currentSlot )
                ]
              )
            ]
    Al.MaxTxSizeUTxO actualSize maxSize ->
        encodeObject
            [ ( "txTooLarge", encodeObject
                [ ( "maximumSize", encodeInteger maxSize )
                , ( "actualSize", encodeInteger actualSize )
                ]
              )
            ]
    Al.InputSetEmptyUTxO ->
        encodeObject
            [ ( "missingAtLeastOneInputUtxo", encodeNull )
            ]
    Al.FeeTooSmallUTxO required actual ->
        encodeObject
            [ ( "feeTooSmall", encodeObject
                [ ( "requiredFee", encodeCoin required )
                , ( "actualFee", encodeCoin actual )
                ]
              )
            ]
    Al.ValueNotConservedUTxO consumed produced ->
        encodeObject
            [ ( "valueNotConserved", encodeObject
                [ ( "consumed", Jen.encodeValue consumed )
                , ( "produced", Jen.encodeValue produced )
                ]
              )
            ]
    Al.WrongNetwork expected invalidAddrs ->
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
    Al.WrongNetworkWithdrawal expected invalidAccts ->
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
    Al.WrongNetworkInTxBody expected actual ->
        encodeObject
            [ ( "networkMismatch", encodeObject
                [ ( "expectedNetwork"
                  , Sophie.encodeNetwork expected
                  )
                , ( "invalidEntities"
                  , Sophie.encodeEntities "transactionBody" Sophie.encodeNetwork [actual]
                  )
                ]
              )
            ]
    Al.OutputTooSmallUTxO outs ->
        encodeObject
            [ ( "outputTooSmall"
              , encodeFoldable encodeTxOut outs
              )
            ]
    Al.OutputBootAddrAttrsTooBig outs ->
        encodeObject
            [ ( "addressAttributesTooLarge"
              , encodeFoldable Sophie.encodeAddress ((\(Al.TxOut addr _ _) -> addr) <$> outs)
              )
            ]
    Al.TriesToForgeBCC ->
        encodeObject
            [ ( "triesToForgeBcc", encodeNull )
            ]
    Al.OutputTooBigUTxO outs ->
        encodeObject
            [ ( "tooManyAssetsInOutput"
              , encodeFoldable (\(_, _, o) -> encodeTxOut o)  outs
              )
            ]
    Al.NoCollateralInputs ->
        encodeObject
            [ ( "missingCollateralInputs", encodeNull )
            ]
    Al.InsufficientCollateral actual required ->
        encodeObject
            [ ( "collateralTooSmall", encodeObject
                [ ( "requiredCollateral", encodeCoin required )
                , ( "actualCollateral", encodeCoin actual )
                ]
              )
            ]
    Al.ScriptsNotPaidUTxO utxo ->
        encodeObject
            [ ( "collateralIsScript", encodeUtxo utxo )
            ]
    Al.CollateralContainsNonBCC value ->
        encodeObject
            [ ( "collateralHasNonBccAssets", Jen.encodeValue value )
            ]
    Al.TooManyCollateralInputs maxInputs actualInputs ->
        encodeObject
            [ ( "tooManyCollateralInputs", encodeObject
                [ ( "maximumCollateralInputs", encodeNatural maxInputs )
                , ( "actualCollateralInputs", encodeNatural actualInputs )
                ]
              )
            ]
    Al.ExUnitsTooBigUTxO maxUnit actualUnit ->
        encodeObject
            [ ( "executionUnitsTooLarge", encodeObject
                [ ( "maximumExecutionUnits", encodeExUnits maxUnit )
                , ( "actualExecutionUnits", encodeExUnits actualUnit )
                ]
              )
            ]
    Al.OutsideForecast slot ->
        encodeObject
            [ ( "outsideForecast", encodeSlotNo slot )
            ]
    Al.UtxosFailure e ->
        encodeUtxosPredicateFailure e

encodeUtxosPredicateFailure
    :: Crypto crypto
    => Al.UtxosPredicateFailure (AurumEra crypto)
    -> Json
encodeUtxosPredicateFailure = \case
    Al.ValidationTagMismatch{} ->
        encodeObject
            [ ( "validationTagMismatch", encodeNull )
            ]
    Al.CollectErrors errors ->
        encodeObject
            [ ( "collectErrors", encodeFoldable encodeCollectError errors )
            ]
    Al.UpdateFailure e ->
        Sophie.encodeUpdateFailure e

encodeScriptIntegrityHash
    :: Crypto crypto
    => Al.ScriptIntegrityHash crypto
    -> Json
encodeScriptIntegrityHash =
    Sophie.encodeHash . Ledger.extractHash

encodeScriptFailure
    :: Crypto crypto
    => Al.Tools.ScriptFailure crypto
    -> Json
encodeScriptFailure = \case
    Al.Tools.RedeemerNotNeeded ptr ->
        encodeObject
            [ ( "extraRedeemers"
              , encodeFoldable (encodeText . stringifyRdmrPtr) [ptr]
              )
            ]
    Al.Tools.MissingScript ptr ->
        encodeObject
            [ ( "missingRequiredScripts"
              , encodeObject
                  [ ( "missing"
                    , encodeFoldable (encodeText . stringifyRdmrPtr) [ptr]
                    )
                  ]
              )
            ]
    Al.Tools.MissingDatum h ->
        encodeObject
            [ ( "missingRequiredDatums"
              , encodeObject
                  [ ( "missing"
                    , encodeFoldable encodeDataHash [h]
                    )
                  ]
              )
            ]
    Al.Tools.ValidationFailedV1 err traces ->
        encodeObject
            [ ( "validatorFailed"
              , encodeObject
                  [ ( "error"
                    , encodeText (show (pretty err))
                    )
                  , ( "traces"
                    , encodeFoldable encodeText traces
                    )
                  ]
              )
            ]
    Al.Tools.ValidationFailedV2 err traces ->
        encodeObject
            [ ( "validatorFailed"
              , encodeObject
                  [ ( "error"
                    , encodeText (show (pretty err))
                    )
                  , ( "traces"
                    , encodeFoldable encodeText traces
                    )
                  ]
              )
            ]
    Al.Tools.UnknownTxIn i ->
        encodeObject
            [ ( "unknownInputReferencedByRedeemer"
              , Sophie.encodeTxIn i
              )
            ]
    Al.Tools.InvalidTxIn i ->
        encodeObject
            [ ( "nonScriptInputReferencedByRedeemer"
              , Sophie.encodeTxIn i
              )
            ]
    Al.Tools.IncompatibleBudget budget ->
        encodeObject
            [ ( "illFormedExecutionBudget"
              , encodeMaybe encodeExUnits (Al.exBudgetToExUnits budget)
              )
            ]
    Al.Tools.NoCostModel lang ->
        encodeObject
            [ ( "noCostModelForLanguage"
              , encodeLanguage lang
              )
            ]

encodeWitnessSet
    :: Crypto crypto
    => Al.TxWitness (AurumEra crypto)
    -> Json
encodeWitnessSet x = encodeObject
    [ ( "signatures"
      , Sophie.encodeWitVKeys (Al.txwitsVKey x)
      )
    , ( "scripts"
      , encodeMap Sophie.stringifyScriptHash encodeScript (Al.txscripts x)
      )
    , ( "datums"
      , encodeMap stringifyDataHash encodeData (Al.unTxDats $ Al.txdats x)
      )
    , ( "redeemers"
      , encodeRedeemers (Al.txrdmrs x)
      )
    , ( "bootstrap"
      , encodeFoldable Sophie.encodeBootstrapWitness (Al.txwitsBoot x)
      )
    ]

--
-- Conversion To Text
--

stringifyDataHash
    :: Crypto crypto
    => Al.DataHash crypto
    -> Text
stringifyDataHash (Ledger.extractHash -> (CC.UnsafeHash h)) =
    encodeBase16 (fromShort h)

stringifyLanguage
    :: Al.Language
    -> Text
stringifyLanguage = \case
    Al.PlutusV1 -> "plutus:v1"
    Al.PlutusV2 -> "plutus:v2"

stringifyRdmrPtr
    :: Al.RdmrPtr
    -> Text
stringifyRdmrPtr (Al.RdmrPtr tag ptr) =
    stringifyTag tag <> ":" <> show ptr
  where
    stringifyTag
        :: Al.Tag
        -> Text
    stringifyTag = \case
        Al.Spend -> "spend"
        Al.Mint -> "mint"
        Al.Cert -> "certificate"
        Al.Rewrd -> "withdrawal"
