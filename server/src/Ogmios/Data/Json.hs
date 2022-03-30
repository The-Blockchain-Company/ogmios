--  This Source Code Form is subject to the terms of the Mozilla Public
--  License, v. 2.0. If a copy of the MPL was not distributed with this
--  file, You can obtain one at http://mozilla.org/MPL/2.0/.

{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeApplications #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Ogmios.Data.Json
    ( Json
    , SerializationMode (..)
    , ViaEncoding (..)
    , jsonToByteString
    , FromJSON
    , ToJSON
    , decodeWith
    , inefficientEncodingToValue

      -- * Encoders
    , encodeAcquireFailure
    , encodeBlock
    , Aurum.encodeExUnits
    , encodePoint
    , Aurum.encodeScriptFailure
    , encodeSerializedTx
    , encodeSubmitTxError
    , encodeTip
    , encodeTx
    , encodeTxId
    , Sophie.encodeTxIn
    , Aurum.stringifyRdmrPtr
    , Aurum.encodeUtxo

      -- * Decoders
    , decodeOneEraHash
    , decodePoint
    , decodeSerializedTx
    , decodeTip
    , decodeTxId
    , decodeUtxo
    ) where

import Ogmios.Data.Json.Prelude

import Cardano.Binary
    ( DecoderError, FromCBOR (..), ToCBOR (..), decodeFull )
import Cardano.Crypto.Hash
    ( hashFromBytes )
import Cardano.Crypto.Hashing
    ( decodeHash, hashToBytes )
import Cardano.Ledger.Aurum.TxBody
    ( TxOut )
import Cardano.Ledger.Crypto
    ( Crypto )
import Cardano.Ledger.Sophie.API
    ( ApplyTxError (..), OptimumCrypto )
import Cardano.Ledger.Sophie.UTxO
    ( UTxO (..) )
import Cardano.Ledger.TxIn
    ( TxIn )
import Cardano.Network.Protocol.NodeToClient
    ( GenTx, GenTxId, SerializedTx, SubmitTxError )
import Cardano.Slotting.Block
    ( BlockNo (..) )
import Cardano.Slotting.Slot
    ( SlotNo (..), WithOrigin (..) )
import Formatting.Buildable
    ( build )
import Ogmios.Data.Json.Query
    ( decodeTxIn
    , decodeTxOut
    , encodeEraMismatch
    , encodeOneEraHash
    , encodePoint
    )
import Shardagnostic.Consensus.Cole.Ledger.Block
    ( ColeBlock (..) )
import Shardagnostic.Consensus.Cole.Ledger.Mempool
    ( encodeColeGenTx )
import Shardagnostic.Consensus.Cardano.Block
    ( CardanoBlock
    , CardanoEras
    , GenTx (..)
    , HardForkApplyTxErr (..)
    , HardForkBlock (..)
    , TxId (..)
    )
import Shardagnostic.Consensus.HardFork.Combinator
    ( OneEraHash (..) )
import Shardagnostic.Consensus.Sophie.Eras
    ( AurumEra, JenEra )
import Shardagnostic.Consensus.Sophie.Ledger
    ( SophieBlock )
import Shardagnostic.Consensus.Sophie.Ledger.Mempool
    ( GenTx (..), TxId (..) )
import Shardagnostic.Network.Block
    ( Point (..), Tip (..), genesisPoint, wrapCBORinCBOR )
import Shardagnostic.Network.Point
    ( Block (..) )
import Shardagnostic.Network.Protocol.LocalStateQuery.Type
    ( AcquireFailure (..) )

import qualified Cardano.Ledger.SafeHash as Ledger
import qualified Cardano.Ledger.TxIn as Ledger
import qualified Codec.CBOR.Encoding as Cbor
import qualified Codec.CBOR.Write as Cbor
import qualified Data.Map.Strict as Map
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Builder as TL

import qualified Data.Aeson as Json
import qualified Data.Aeson.Types as Json

import qualified Ogmios.Data.Json.Evie as Evie
import qualified Ogmios.Data.Json.Aurum as Aurum
import qualified Ogmios.Data.Json.Cole as Cole
import qualified Ogmios.Data.Json.Jen as Jen
import qualified Ogmios.Data.Json.Sophie as Sophie

--
-- Encoders
--

encodeAcquireFailure
    :: AcquireFailure
    -> Json
encodeAcquireFailure = \case
    AcquireFailurePointTooOld ->
        encodeText "pointTooOld"
    AcquireFailurePointNotOnChain ->
        encodeText "pointNotOnChain"

encodeBlock
    :: Crypto crypto
    => SerializationMode
    -> CardanoBlock crypto
    -> Json
encodeBlock mode = \case
    BlockCole blk -> encodeObject
        [ ( "cole"
          , Cole.encodeABlockOrBoundary mode (coleBlockRaw blk)
          )
        ]
    BlockSophie blk -> encodeObject
        [ ( "sophie"
          , Sophie.encodeBlock mode blk
          )
        ]
    BlockAllegra blk -> encodeObject
        [ ( "evie"
          , Evie.encodeBlock mode blk
          )
        ]
    BlockJen blk -> encodeObject
        [ ( "jen"
          , Jen.encodeBlock mode blk
          )
        ]
    BlockAurum blk -> encodeObject
        [ ( "aurum"
          , Aurum.encodeBlock mode blk
          )
        ]

encodeSubmitTxError
    :: Crypto crypto
    => SubmitTxError (CardanoBlock crypto)
    -> Json
encodeSubmitTxError = \case
    ApplyTxErrCole e ->
        Cole.encodeApplyMempoolPayloadErr e
    ApplyTxErrSophie (ApplyTxError xs) ->
        encodeList Sophie.encodeLedgerFailure xs
    ApplyTxErrAllegra (ApplyTxError xs) ->
        encodeList Evie.encodeLedgerFailure xs
    ApplyTxErrJen (ApplyTxError xs) ->
        encodeList Jen.encodeLedgerFailure xs
    ApplyTxErrAurum (ApplyTxError xs) ->
        encodeList Aurum.encodeLedgerFailure xs
    ApplyTxErrWrongEra e ->
        encodeList encodeEraMismatch [ e ]

encodeSerializedTx
    :: OptimumCrypto crypto
    => SerializedTx (CardanoBlock crypto)
    -> Json
encodeSerializedTx = \case
    GenTxCole tx ->
        encodeByteStringBase16 $ Cbor.toStrictByteString $ encodeColeGenTx tx
    GenTxSophie tx ->
        encodeByteStringBase16 $ Cbor.toStrictByteString $ toCBOR tx
    GenTxAllegra tx ->
        encodeByteStringBase16 $ Cbor.toStrictByteString $ toCBOR tx
    GenTxJen tx ->
        encodeByteStringBase16 $ Cbor.toStrictByteString $ toCBOR tx
    GenTxAurum tx ->
        encodeByteStringBase16 $ Cbor.toStrictByteString $ toCBOR tx

encodeTip
    :: Tip (CardanoBlock crypto)
    -> Json
encodeTip = \case
    TipGenesis -> encodeText "origin"
    Tip slot hash blockNo -> encodeObject
        [ ( "slot"
          , encodeSlotNo slot
          )
        , ( "hash"
          , encodeOneEraHash hash
          )
        , ( "blockNo"
          , encodeBlockNo blockNo
          )
        ]

encodeTx
    :: forall crypto.
        ( Crypto crypto
        )
    => SerializationMode
    -> GenTx (CardanoBlock crypto)
    -> Json
encodeTx mode = \case
    GenTxAurum (SophieTx _ x) ->
        Aurum.encodeTx mode x
    GenTxJen (SophieTx _ x) ->
        Jen.encodeTx mode x
    GenTxAllegra (SophieTx _ x) ->
        Evie.encodeTx mode x
    GenTxSophie (SophieTx _ x) ->
        Sophie.encodeTx mode x
    GenTxCole _ ->
        error "encodeTx: unsupported Cole transaction."

encodeTxId
    :: Crypto crypto
    => GenTxId (CardanoBlock crypto)
    -> Json
encodeTxId = \case
    GenTxIdAurum (SophieTxId x) ->
        Sophie.encodeTxId x
    GenTxIdJen (SophieTxId x) ->
        Sophie.encodeTxId x
    GenTxIdAllegra (SophieTxId x) ->
        Sophie.encodeTxId x
    GenTxIdSophie (SophieTxId x) ->
        Sophie.encodeTxId x
    GenTxIdCole _ ->
        error "encodeTxId: unsupported Cole transaction."

--
-- Decoders
--

decodeOneEraHash
    :: Text
    -> Json.Parser (OneEraHash (CardanoEras crypto))
decodeOneEraHash =
    either (const mempty) (pure . OneEraHash . toShort . hashToBytes) . decodeHash

decodePoint
    :: Json.Value
    -> Json.Parser (Point (CardanoBlock crypto))
decodePoint json =
    parseOrigin json <|> parsePoint json
  where
    parseOrigin = Json.withText "Point" $ \case
        txt | txt == "origin" -> pure genesisPoint
        _ -> empty

    parsePoint = Json.withObject "Point" $ \obj -> do
        slot <- obj .: "slot"
        hash <- obj .: "hash" >>= decodeOneEraHash
        pure $ Point $ At $ Block (SlotNo slot) hash

decodeSerializedTx
    :: forall crypto. OptimumCrypto crypto
    => Json.Value
    -> Json.Parser (SerializedTx (CardanoBlock crypto))
decodeSerializedTx = Json.withText "Tx" $ \(encodeUtf8 -> utf8) -> do
    bytes <- decodeBase16 utf8 <|> decodeBase64 utf8 <|> invalidEncodingError
    -- NOTE: Avoiding 'asum' here because it generates poor errors on failures
    deserialiseCBOR GenTxJen (wrap bytes)
        <|> deserialiseCBOR GenTxJen (fromStrict bytes)
        <|> deserialiseCBOR GenTxAurum (wrap bytes)
        <|> deserialiseCBOR GenTxAurum (fromStrict bytes)
  where
    invalidEncodingError :: Json.Parser a
    invalidEncodingError =
        fail "failed to decode payload from base64 or base16."

    -- Cardano tools have a tendency to wrap cbor in cbor (e.g bcc-cli).
    -- In particular, a `GenTx` is expected to be prefixed with a cbor tag
    -- `24` and serialized as CBOR bytes `58xx`.
    wrap :: ByteString -> LByteString
    wrap = Cbor.toLazyByteString . wrapCBORinCBOR Cbor.encodePreEncoded

    deserialiseCBOR
        :: forall era.
            ( Or
                (era ~ LastElem (CardanoEras crypto))
                (era ~ SophieBlock (JenEra crypto))
            , FromCBOR (GenTx era)
            )
        => (GenTx era -> GenTx (CardanoBlock crypto))
        -> LByteString
        -> Json.Parser (GenTx (CardanoBlock crypto))
    deserialiseCBOR mk =
        either (fail . prettyDecoderError) (pure . mk)
        .
        decodeFull
      where
        _compilerWarning = keepRedundantConstraint
            (Proxy @(Or
                (era ~ LastElem (CardanoEras crypto))
                (era ~ SophieBlock (JenEra crypto))
            ))

        prettyDecoderError :: DecoderError -> String
        prettyDecoderError =
            toString
                . TL.replace
                    (toLazy $ label (Proxy @(GenTx era)))
                    "serialised transaction"
                . TL.replace
                    "\n"
                    " "
                . TL.toLazyText
                . build

decodeUtxo
    :: forall crypto. (Crypto crypto)
    => Json.Value
    -> Json.Parser (UTxO (AurumEra crypto))
decodeUtxo v = do
    xs <- Json.parseJSONList v >>= traverse decodeUtxoEntry
    pure $ UTxO (Map.fromList xs)
  where
    decodeUtxoEntry :: Json.Value -> Json.Parser (TxIn crypto, TxOut (AurumEra crypto))
    decodeUtxoEntry =
        Json.parseJSONList >=> \case
            [i, o] ->
                (,) <$> decodeTxIn i <*> decodeTxOut o
            _ ->
                fail
                    "Failed to decode utxo entry. Expected an array of length \
                    \2 as [output-reference, output]"

decodeTip
    :: Json.Value
    -> Json.Parser (Tip (CardanoBlock crypto))
decodeTip json =
    parseOrigin json <|> parseTip json
  where
    parseOrigin = Json.withText "Tip" $ \case
        txt | txt == "origin" -> pure TipGenesis
        _ -> empty

    parseTip = Json.withObject "Tip" $ \obj -> do
        slot <- obj .: "slot"
        hash <- obj .: "hash" >>= decodeOneEraHash
        blockNo <- obj .: "blockNo"
        pure $ Tip (SlotNo slot) hash (BlockNo blockNo)

decodeTxId
    :: forall crypto. OptimumCrypto crypto
    => Json.Value
    -> Json.Parser (GenTxId (CardanoBlock crypto))
decodeTxId = Json.withText "TxId" $ \(encodeUtf8 -> utf8) -> do
    bytes <- decodeBase16 utf8
    case hashFromBytes bytes of
        Nothing ->
            fail "couldn't interpret bytes as blake2b-256 digest."
        Just h ->
            pure $ GenTxIdAurum $ SophieTxId $ Ledger.TxId (Ledger.unsafeMakeSafeHash h)
