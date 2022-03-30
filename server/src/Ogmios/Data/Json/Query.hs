--  This Source Code Form is subject to the terms of the Mozilla Public
--  License, v. 2.0. If a copy of the MPL was not distributed with this
--  file, You can obtain one at http://mozilla.org/MPL/2.0/.

{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}

module Ogmios.Data.Json.Query
    ( -- * Types
      Query (..)
    , QueryInEra
    , SomeQuery (..)
    , QueryResult

      -- ** Eras
    , SophieBasedEra (..)
    , SomeSophieEra (..)
    , fromEraIndex

      -- ** Types in queries
    , RewardAccounts
    , Delegations
    , Interpreter
    , Sh.RewardProvenance
    , Sh.RewardProvenancePool
    , RewardProvenance'
    , Sh.Api.RewardInfoPool
    , Sh.Api.RewardParams
    , Sh.Desirability
    , Sh.PoolParams

      -- * Encoders
    , encodeBound
    , encodeDelegationsAndRewards
    , encodeDesirabilities
    , encodeEpochNo
    , encodeEraMismatch
    , encodeInterpreter
    , encodeMismatchEraInfo
    , encodeNonMyopicMemberRewards
    , encodeOneEraHash
    , encodePoint
    , encodePoolDistr
    , encodePoolParameters
    , encodeRewardInfoPool
    , encodeRewardInfoPools
    , encodeRewardProvenance

      -- * Decoders
    , decodeTxIn
    , decodeTxOut

      -- * Parsers
    , parseGetBlockHeight
    , parseGetChainTip
    , parseGetCurrentPParams
    , parseGetEpochNo
    , parseGetEraStart
    , parseGetFilteredDelegationsAndRewards
    , parseGetGenesisConfig
    , parseGetInterpreter
    , parseGetLedgerTip
    , parseGetNonMyopicMemberRewards
    , parseGetPoolIds
    , parseGetPoolParameters
    , parseGetPoolsRanking
    , parseGetProposedPParamsUpdates
    , parseGetRewardInfoPools
    , parseGetRewardProvenance
    , parseGetStakeDistribution
    , parseGetSystemStart
    , parseGetUTxO
    , parseGetUTxOByAddress
    , parseGetUTxOByTxIn
    ) where

import Ogmios.Data.Json.Prelude

import Cardano.Api
    ( SophieBasedEra (..) )
import Cardano.Crypto.Hash
    ( pattern UnsafeHash, hashFromBytes, hashFromTextAsHex )
import Cardano.Ledger.Crypto
    ( Crypto, HASH )
import Cardano.Ledger.Keys
    ( KeyRole (..) )
import Cardano.Ledger.SafeHash
    ( unsafeMakeSafeHash )
import Cardano.Slotting.Slot
    ( EpochNo (..), WithOrigin (..) )
import Codec.Serialise
    ( deserialise, serialise )
import Data.Aeson
    ( toJSON )
import Data.SOP.Strict
    ( NS (..) )
import Shardagnostic.Consensus.BlockchainTime
    ( SystemStart (..) )
import Shardagnostic.Consensus.Cardano.Block
    ( BlockQuery (..), CardanoBlock, CardanoEras )
import Shardagnostic.Consensus.HardFork.Combinator
    ( EraIndex (..), MismatchEraInfo, OneEraHash (..) )
import Shardagnostic.Consensus.HardFork.Combinator.AcrossEras
    ( EraMismatch (..), mkEraMismatch )
import Shardagnostic.Consensus.HardFork.Combinator.Ledger.Query
    ( QueryAnytime (..) )
import Shardagnostic.Consensus.HardFork.History.EraParams
    ( EraParams (..), SafeZone (..) )
import Shardagnostic.Consensus.HardFork.History.Qry
    ( Interpreter )
import Shardagnostic.Consensus.HardFork.History.Summary
    ( Bound (..), EraEnd (..), EraSummary (..), Summary (..) )
import Shardagnostic.Consensus.Sophie.Eras
    ( AllegraEra, AurumEra, JenEra, SophieEra )
import Shardagnostic.Consensus.Sophie.Ledger.Block
    ( SophieBlock (..), SophieHash (..) )
import Shardagnostic.Consensus.Sophie.Ledger.Config
    ( CompactGenesis, getCompactGenesis )
import Shardagnostic.Consensus.Sophie.Ledger.Query
    ( BlockQuery (..), NonMyopicMemberRewards (..) )
import Shardagnostic.Network.Block
    ( BlockNo, pattern BlockPoint, pattern GenesisPoint, Point (..) )
import Shardagnostic.Network.Point
    ( Block (..) )

import qualified Codec.Binary.Bech32 as Bech32
import qualified Data.Aeson as Json
import qualified Data.Aeson.Types as Json
import qualified Data.HashMap.Strict as HMap
import qualified Data.Map.Merge.Strict as Map
import qualified Data.Text as T

import qualified Shardagnostic.Consensus.HardFork.Combinator.Ledger.Query as LSQ
import qualified Shardagnostic.Consensus.Ledger.Query as LSQ

import qualified Cardano.Crypto.Hash.Class as CC

import qualified Cardano.Ledger.Address as Ledger
import qualified Cardano.Ledger.Aurum.Data as Ledger.Aurum
import qualified Cardano.Ledger.Aurum.TxBody as Ledger.Aurum
import qualified Cardano.Ledger.BaseTypes as Ledger
import qualified Cardano.Ledger.Coin as Ledger
import qualified Cardano.Ledger.Core as Ledger
import qualified Cardano.Ledger.Credential as Ledger
import qualified Cardano.Ledger.Era as Ledger
import qualified Cardano.Ledger.Hashes as Ledger
import qualified Cardano.Ledger.Keys as Ledger
import qualified Cardano.Ledger.Jen.Value as Ledger.Jen
import qualified Cardano.Ledger.PoolDistr as Ledger
import qualified Cardano.Ledger.TxIn as Ledger

import qualified Cardano.Protocol.TOptimum.BHeader as TOptimum

import qualified Cardano.Ledger.Sophie.API.Wallet as Sh.Api
import qualified Cardano.Ledger.Sophie.PParams as Sh
import qualified Cardano.Ledger.Sophie.RewardProvenance as Sh
import qualified Cardano.Ledger.Sophie.TxBody as Sh
import qualified Cardano.Ledger.Sophie.UTxO as Sh

import qualified Ogmios.Data.Json.Evie as Evie
import qualified Ogmios.Data.Json.Aurum as Aurum
import qualified Ogmios.Data.Json.Jen as Jen
import qualified Ogmios.Data.Json.Sophie as Sophie

--
-- Types
--

data Query (f :: Type -> Type) block = Query
    { rawQuery :: Json.Value
    , queryInEra :: QueryInEra f block
    } deriving (Generic)

type QueryInEra f block =
    SomeSophieEra -> Maybe (SomeQuery f block)

data SomeQuery (f :: Type -> Type) block = forall result. SomeQuery
    { query :: LSQ.Query block result
    , encodeResult :: SerializationMode -> result -> Json
    , genResult :: Proxy result -> f result
    }

instance Crypto crypto => FromJSON (Query Proxy (CardanoBlock crypto)) where
    parseJSON = choice "query"
        [ \raw -> Query raw <$> parseGetBlockHeight id raw
        , \raw -> Query raw <$> parseGetChainTip id raw
        , \raw -> Query raw <$> parseGetCurrentPParams (const id) raw
        , \raw -> Query raw <$> parseGetEpochNo id raw
        , \raw -> Query raw <$> parseGetEraStart id raw
        , \raw -> Query raw <$> parseGetFilteredDelegationsAndRewards id raw
        , \raw -> Query raw <$> parseGetGenesisConfig (const id) raw
        , \raw -> Query raw <$> parseGetInterpreter id raw
        , \raw -> Query raw <$> parseGetLedgerTip (const id) raw
        , \raw -> Query raw <$> parseGetNonMyopicMemberRewards id raw
        , \raw -> Query raw <$> parseGetPoolIds id raw
        , \raw -> Query raw <$> parseGetPoolParameters id raw
        , \raw -> Query raw <$> parseGetPoolsRanking id raw
        , \raw -> Query raw <$> parseGetProposedPParamsUpdates (const id) raw
        , \raw -> Query raw <$> parseGetRewardInfoPools id raw
        , \raw -> Query raw <$> parseGetRewardProvenance id raw
        , \raw -> Query raw <$> parseGetStakeDistribution id raw
        , \raw -> Query raw <$> parseGetSystemStart id raw
        , \raw -> Query raw <$> parseGetUTxO (const id) raw
        , \raw -> Query raw <$> parseGetUTxOByAddress (const id) raw
        , \raw -> Query raw <$> parseGetUTxOByTxIn (const id) raw
        ]

type QueryResult crypto result =
    Either (MismatchEraInfo (CardanoEras crypto)) result

type GenResult crypto f t =
    Proxy (QueryResult crypto t) -> f (QueryResult crypto t)

type Delegations crypto =
    Map (Ledger.Credential 'Staking crypto) (Ledger.KeyHash 'StakePool crypto)

type RewardAccounts crypto =
    Map (Ledger.Credential 'Staking crypto) Coin

type RewardProvenance' crypto =
    ( Sh.Api.RewardParams
    , Map (Ledger.KeyHash 'StakePool crypto) (Sh.Api.RewardInfoPool)
    )


--
-- SomeSophieEra
--

data SomeSophieEra =
    forall era. SomeSophieEra (SophieBasedEra era)

deriving instance Show SomeSophieEra

instance ToJSON SomeSophieEra where
    toJSON = \case
        SomeSophieEra SophieBasedEraSophie -> toJSON @Text "Sophie"
        SomeSophieEra SophieBasedEraAllegra -> toJSON @Text "Evie"
        SomeSophieEra SophieBasedEraJen -> toJSON @Text "Jen"
        SomeSophieEra SophieBasedEraAurum -> toJSON @Text "Aurum"

-- | Convert an 'EraIndex' to a Sophie-based era.
fromEraIndex
    :: forall crypto. ()
    => EraIndex (CardanoEras crypto)
    -> Maybe SomeSophieEra
fromEraIndex = \case
    EraIndex             Z{}     -> Nothing
    EraIndex          (S Z{})    -> Just (SomeSophieEra SophieBasedEraSophie)
    EraIndex       (S (S Z{}))   -> Just (SomeSophieEra SophieBasedEraAllegra)
    EraIndex    (S (S (S Z{})))  -> Just (SomeSophieEra SophieBasedEraJen)
    EraIndex (S (S (S (S Z{})))) -> Just (SomeSophieEra SophieBasedEraAurum)

--
-- Encoders
--

encodeBound
    :: Bound
    -> Json
encodeBound bound = encodeObject
    [ ( "time", encodeRelativeTime (boundTime bound) )
    , ( "slot", encodeSlotNo (boundSlot bound) )
    , ( "epoch", encodeEpochNo (boundEpoch bound) )
    ]

encodeDelegationsAndRewards
    :: Crypto crypto
    => SerializationMode
    -> (Delegations crypto, RewardAccounts crypto)
    -> Json
encodeDelegationsAndRewards mode (dlg, rwd) =
    encodeMapWithMode mode Sophie.stringifyCredential id merge
  where
    merge = Map.merge whenDlgMissing whenRwdMissing whenBothPresent dlg rwd

    whenDlgMissing = Map.mapMaybeMissing
        (\_ v -> Just $ encodeObject
            [ ( "delegate", Sophie.encodePoolId v )
            ]
        )
    whenRwdMissing = Map.mapMaybeMissing
        (\_ v -> Just $ encodeObject
            [ ( "rewards", encodeCoin v )
            ]
        )
    whenBothPresent = Map.zipWithAMatched
        (\_ x y -> pure $ encodeObject
            [ ( "delegate", Sophie.encodePoolId x )
            , ( "rewards", encodeCoin y )
            ]
        )

encodeDesirabilities
    :: Crypto crypto
    => SerializationMode
    -> Sh.RewardProvenance crypto
    -> Json
encodeDesirabilities mode rp =
    encodeMapWithMode mode Sophie.stringifyPoolId encodeDesirability (Sh.desirabilities rp)
  where
    encodeDesirability
        :: Sh.Desirability
        -> Json
    encodeDesirability d =
        encodeObject
            [ ( "score", encodeDouble (Sh.desirabilityScore d) )
            , ( "estimatedHitRate", encodeDouble (Sh.desirabilityScore d) )
            ]

encodeEraEnd
    :: EraEnd
    -> Json
encodeEraEnd = \case
    EraEnd bound ->
        encodeBound bound
    EraUnbounded ->
        encodeNull

encodeEraMismatch
    :: EraMismatch
    -> Json
encodeEraMismatch x = encodeObject
    [ ( "eraMismatch", encodeObject
        [ ( "ledgerEra"
          , encodeText (ledgerEraName x)
          )
        , ( "queryEra"
          , encodeText (otherEraName x)
          )
        ]
      )
    ]

encodeEraParams
    :: EraParams
    -> Json
encodeEraParams x = encodeObject
    [ ( "epochLength", encodeEpochSize (eraEpochSize x) )
    , ( "slotLength", encodeSlotLength (eraSlotLength x) )
    , ( "safeZone", encodeSafeZone (eraSafeZone x) )
    ]

encodeEraSummary
    :: EraSummary
    -> Json
encodeEraSummary x = encodeObject
    [ ( "start", encodeBound (eraStart x) )
    , ( "end", encodeEraEnd (eraEnd x) )
    , ( "parameters", encodeEraParams (eraParams x) )
    ]

encodeInterpreter
    :: forall crypto eras. (eras ~ CardanoEras crypto)
    => Interpreter eras
    -> Json
encodeInterpreter (deserialise @(Summary eras). serialise -> Summary eraSummaries) =
    encodeFoldable encodeEraSummary (eraSummaries)

encodeMismatchEraInfo
    :: MismatchEraInfo (CardanoEras crypto)
    -> Json
encodeMismatchEraInfo =
    encodeEraMismatch . mkEraMismatch

encodeNonMyopicMemberRewards
    :: Crypto crypto
    => SerializationMode
    -> NonMyopicMemberRewards crypto
    -> Json
encodeNonMyopicMemberRewards mode (NonMyopicMemberRewards nonMyopicMemberRewards) =
    encodeMapWithMode mode encodeKey encodeVal nonMyopicMemberRewards
  where
    encodeKey = either Sophie.stringifyCoin Sophie.stringifyCredential
    encodeVal = encodeMapWithMode mode Sophie.stringifyPoolId encodeCoin

encodeOneEraHash
    :: OneEraHash eras
    -> Json
encodeOneEraHash =
    encodeShortByteString encodeByteStringBase16 . getOneEraHash

encodePoint
    :: Point (CardanoBlock crypto)
    -> Json
encodePoint = \case
    Point Origin -> encodeText "origin"
    Point (At x) -> encodeObject
        [ ( "slot"
          , encodeSlotNo (blockPointSlot x)
          )
        , ( "hash"
          , encodeOneEraHash (blockPointHash x)
          )
        ]

encodePoolDistr
    :: forall crypto. Crypto crypto
    => SerializationMode
    -> Ledger.PoolDistr crypto
    -> Json
encodePoolDistr mode
    = encodeMapWithMode mode Sophie.stringifyPoolId encodeIndividualPoolStake
    . Ledger.unPoolDistr
  where
    encodeIndividualPoolStake
        :: Ledger.IndividualPoolStake crypto
        -> Json
    encodeIndividualPoolStake x = encodeObject
        [ ( "stake"
          , encodeRational (Ledger.individualPoolStake x)
          )
        , ( "vrf"
          , Sophie.encodeHash (Ledger.individualPoolStakeVrf x)
          )
        ]

encodePoolParameters
    :: Crypto crypto
    => SerializationMode
    -> Map (Ledger.KeyHash 'StakePool crypto) (Sh.PoolParams crypto)
    -> Json
encodePoolParameters mode =
    encodeMapWithMode mode Sophie.stringifyPoolId Sophie.encodePoolParams

encodeRewardInfoPool
    :: Sh.Api.RewardInfoPool
    -> Json
encodeRewardInfoPool info =
    encodeObject
        [ ( "stake"
          , encodeCoin (Sh.Api.stake info)
          )
        , ( "ownerStake"
          , encodeCoin (Sh.Api.ownerStake info)
          )
        , ( "approximatePerformance"
          , encodeDouble (Sh.Api.performanceEstimate info)
          )
        , ( "poolParameters"
          , encodeObject
            [ ( "cost"
              , encodeCoin (Sh.Api.cost info)
              )
            , ( "margin"
              , encodeUnitInterval (Sh.Api.margin info)
              )
            , ( "pledge"
              , encodeCoin (Sh.Api.ownerPledge info)
              )
            ]
          )
        ]

encodeRewardInfoPools
    :: Crypto crypto
    => RewardProvenance' crypto
    -> Json
encodeRewardInfoPools (rp, pools) =
    encodeObject
        [ ( "desiredNumberOfPools"
          , encodeNatural (Sh.Api.nOpt rp)
          )
        , ( "poolInfluence"
          , encodeNonNegativeInterval (Sh.Api.a0 rp)
          )
        , ( "totalRewards"
          , encodeCoin (Sh.Api.rPot rp)
          )
        , ( "activeStake"
          , encodeCoin (Sh.Api.totalStake rp)
          )
        , ( "pools"
          , encodeMap Sophie.stringifyPoolId encodeRewardInfoPool pools
          )
        ]

encodeRewardProvenance
    :: forall crypto. Crypto crypto
    => SerializationMode
    -> Sh.RewardProvenance crypto
    -> Json
encodeRewardProvenance mode rp =
    encodeObjectWithMode mode
        [ ( "epochLength"
          , encodeWord64 (Sh.spe rp)
          )
        , ( "decentralizationParameter"
          , encodeRational (Sh.d rp)
          )
        , ( "maxEntropicSupply"
          , encodeCoin (Sh.maxLL rp)
          )
        , ( "totalMintedBlocks"
          , encodeInteger (Sh.blocksCount rp)
          )
        , ( "totalExpectedBlocks"
          , encodeInteger (Sh.expBlocks rp)
          )
        , ( "incentive"
          , encodeCoin (Sh.deltaR1 rp)
          )
        , ( "rewardsGap"
          , encodeCoin (Sh.deltaR2 rp)
          )
        , ( "availableRewards"
          , encodeCoin (Sh.r rp)
          )
        , ( "totalRewards"
          , encodeCoin (Sh.rPot rp)
          )
        , ( "treasuryTax"
          , encodeCoin (Sh.deltaT1 rp)
          )
        , ( "activeStake"
          , encodeCoin (Sh.activeStake rp)
          )
        ]
        [ ( "pools"
          , encodeMap Sophie.stringifyPoolId encodeRewardProvenancePool (Sh.pools rp)
          )
        , ( "mintedBlocks"
          , encodeMap Sophie.stringifyPoolId encodeNatural (Ledger.unBlocksMade $ Sh.blocks rp)
          )
        ]
  where
    encodeRewardProvenancePool
        :: Sh.RewardProvenancePool crypto
        -> Json
    encodeRewardProvenancePool rpp =
        encodeObject
            [ ( "totalMintedBlocks"
              , encodeNatural (Sh.poolBlocksP rpp)
              )
            , ( "totalStakeShare"
              , encodeRational (Sh.sigmaP rpp)
              )
            , ( "activeStakeShare"
              , encodeRational (Sh.sigmaAP rpp)
              )
            , ( "ownerStake"
              , encodeCoin (Sh.ownerStakeP rpp)
              )
            , ( "parameters"
              , Sophie.encodePoolParams (Sh.poolParamsP rpp)
              )
            , ( "pledgeRatio"
              , encodeRational (Sh.pledgeRatioP rpp)
              )
            , ( "maxRewards"
              , encodeCoin (Sh.maxPP rpp)
              )
            , ( "apparentPerformance"
              , encodeRational (Sh.appPerfP rpp)
              )
            , ( "totalRewards"
              , encodeCoin (Sh.poolRP rpp)
              )
            , ( "leaderRewards"
              , encodeCoin (Sh.lRewardP rpp)
              )
            ]

encodeSafeZone
    :: SafeZone
    -> Json
encodeSafeZone = \case
    StandardSafeZone k ->
        encodeWord64 k
    UnsafeIndefiniteSafeZone ->
        encodeNull

--
-- Parsers (Queries)
--

parseGetBlockHeight
    :: forall crypto f. ()
    => (Proxy (WithOrigin BlockNo) -> f (WithOrigin BlockNo))
    -> Json.Value
    -> Json.Parser (QueryInEra f (CardanoBlock crypto))
parseGetBlockHeight genResult =
    Json.withText "SomeQuery" $ \text -> do
        guard (text == "blockHeight")
        pure $ const $ Just $ SomeQuery
            { query = LSQ.GetChainBlockNo
            , genResult
            , encodeResult = const (encodeWithOrigin encodeBlockNo)
            }

parseGetChainTip
    :: forall crypto f. ()
    => (Proxy (Point (CardanoBlock crypto)) -> f (Point (CardanoBlock crypto)))
    -> Json.Value
    -> Json.Parser (QueryInEra f (CardanoBlock crypto))
parseGetChainTip genResult =
    Json.withText "SomeQuery" $ \text -> do
        guard (text == "chainTip")
        pure $ const $ Just $ SomeQuery
            { query = LSQ.GetChainPoint
            , genResult
            , encodeResult = const encodePoint
            }

parseGetEraStart
    :: forall crypto f. ()
    => (Proxy (Maybe Bound) -> f (Maybe Bound))
    -> Json.Value
    -> Json.Parser (QueryInEra f (CardanoBlock crypto))
parseGetEraStart genResult =
    Json.withText "SomeQuery" $ \text -> do
        guard (text == "eraStart") $>
            ( \query -> Just $ SomeQuery
                { query
                , genResult
                , encodeResult = const (encodeMaybe encodeBound)
                }
            )
            .
            ( \case
                SomeSophieEra SophieBasedEraSophie ->
                    LSQ.BlockQuery $ QueryAnytimeSophie GetEraStart
                SomeSophieEra SophieBasedEraAllegra ->
                    LSQ.BlockQuery $ QueryAnytimeAllegra GetEraStart
                SomeSophieEra SophieBasedEraJen ->
                    LSQ.BlockQuery $ QueryAnytimeJen GetEraStart
                SomeSophieEra SophieBasedEraAurum ->
                    LSQ.BlockQuery $ QueryAnytimeAurum GetEraStart
            )

parseGetLedgerTip
    :: forall crypto f. (Crypto crypto)
    => (forall era. Typeable era => Proxy era -> GenResult crypto f (Point (SophieBlock era)))
    -> Json.Value
    -> Json.Parser (QueryInEra f (CardanoBlock crypto))
parseGetLedgerTip genResultInEra =
    Json.withText "SomeQuery" $ \text -> do
        guard (text == "ledgerTip") $> \case
            SomeSophieEra SophieBasedEraSophie ->
                Just $ SomeQuery
                { query =
                    LSQ.BlockQuery $ QueryIfCurrentSophie GetLedgerTip
                , encodeResult =
                    const (either encodeMismatchEraInfo (encodePoint . castPoint))
                , genResult =
                    genResultInEra (Proxy @(SophieEra crypto))
                }
            SomeSophieEra SophieBasedEraAllegra ->
                Just $ SomeQuery
                { query =
                    LSQ.BlockQuery $ QueryIfCurrentAllegra GetLedgerTip
                , encodeResult =
                    const (either encodeMismatchEraInfo (encodePoint . castPoint))
                , genResult =
                    genResultInEra (Proxy @(AllegraEra crypto))
                }
            SomeSophieEra SophieBasedEraJen ->
                Just $ SomeQuery
                { query =
                    LSQ.BlockQuery $ QueryIfCurrentJen GetLedgerTip
                , encodeResult =
                    const (either encodeMismatchEraInfo (encodePoint . castPoint))
                , genResult =
                    genResultInEra (Proxy @(JenEra crypto))
                }
            SomeSophieEra SophieBasedEraAurum ->
                Just $ SomeQuery
                { query =
                    LSQ.BlockQuery $ QueryIfCurrentAurum GetLedgerTip
                , encodeResult =
                    const (either encodeMismatchEraInfo (encodePoint . castPoint))
                , genResult =
                    genResultInEra (Proxy @(AurumEra crypto))
                }

parseGetEpochNo
    :: forall crypto f. ()
    => GenResult crypto f EpochNo
    -> Json.Value
    -> Json.Parser (QueryInEra f (CardanoBlock crypto))
parseGetEpochNo genResult =
    Json.withText "SomeQuery" $ \text -> do
        guard (text == "currentEpoch") $>
            ( \query -> Just $ SomeQuery
                { query
                , genResult
                , encodeResult =
                    const (either encodeMismatchEraInfo encodeEpochNo)
                }
            )
            .
            ( \case
                SomeSophieEra SophieBasedEraSophie ->
                    LSQ.BlockQuery $ QueryIfCurrentSophie GetEpochNo
                SomeSophieEra SophieBasedEraAllegra ->
                    LSQ.BlockQuery $ QueryIfCurrentAllegra GetEpochNo
                SomeSophieEra SophieBasedEraJen ->
                    LSQ.BlockQuery $ QueryIfCurrentJen GetEpochNo
                SomeSophieEra SophieBasedEraAurum ->
                    LSQ.BlockQuery $ QueryIfCurrentAurum GetEpochNo
            )

parseGetNonMyopicMemberRewards
    :: forall crypto f. (Crypto crypto)
    => GenResult crypto f (NonMyopicMemberRewards crypto)
    -> Json.Value
    -> Json.Parser (QueryInEra f (CardanoBlock crypto))
parseGetNonMyopicMemberRewards genResult =
    Json.withObject "SomeQuery" $ \obj -> do
        credentials <- decodeCredentials obj
        pure $
            ( \query -> Just $ SomeQuery
                { query
                , genResult
                , encodeResult =
                    either encodeMismatchEraInfo . encodeNonMyopicMemberRewards
                }
            )
            .
            ( \case
                SomeSophieEra SophieBasedEraSophie ->
                    LSQ.BlockQuery $ QueryIfCurrentSophie (GetNonMyopicMemberRewards credentials)
                SomeSophieEra SophieBasedEraAllegra ->
                    LSQ.BlockQuery $ QueryIfCurrentAllegra (GetNonMyopicMemberRewards credentials)
                SomeSophieEra SophieBasedEraJen ->
                    LSQ.BlockQuery $ QueryIfCurrentJen (GetNonMyopicMemberRewards credentials)
                SomeSophieEra SophieBasedEraAurum ->
                    LSQ.BlockQuery $ QueryIfCurrentAurum (GetNonMyopicMemberRewards credentials)
            )
  where
    decodeCredentials
        :: Json.Object
        -> Json.Parser (Set (Either Ledger.Coin (Ledger.Credential 'Staking crypto)))
    decodeCredentials obj = fmap fromList $
        obj .: "nonMyopicMemberRewards" >>= traverse
            (choice "credential"
                [ fmap Left  . decodeCoin
                , fmap Right . decodeCredential
                ]
            )

parseGetFilteredDelegationsAndRewards
    :: forall crypto f. (Crypto crypto)
    => GenResult crypto f (Delegations crypto, RewardAccounts crypto)
    -> Json.Value
    -> Json.Parser (QueryInEra f (CardanoBlock crypto))
parseGetFilteredDelegationsAndRewards genResult =
    Json.withObject "SomeQuery" $ \obj -> do
        credentials <- decodeCredentials obj
        pure $
            ( \query -> Just $ SomeQuery
                { query
                , genResult
                , encodeResult =
                    either encodeMismatchEraInfo . encodeDelegationsAndRewards
                }
            )
            .
            ( \case
                SomeSophieEra SophieBasedEraSophie ->
                    LSQ.BlockQuery $ QueryIfCurrentSophie (GetFilteredDelegationsAndRewardAccounts credentials)
                SomeSophieEra SophieBasedEraAllegra ->
                    LSQ.BlockQuery $ QueryIfCurrentAllegra (GetFilteredDelegationsAndRewardAccounts credentials)
                SomeSophieEra SophieBasedEraJen ->
                    LSQ.BlockQuery $ QueryIfCurrentJen (GetFilteredDelegationsAndRewardAccounts credentials)
                SomeSophieEra SophieBasedEraAurum ->
                    LSQ.BlockQuery $ QueryIfCurrentAurum (GetFilteredDelegationsAndRewardAccounts credentials)
            )
  where
    decodeCredentials
        :: Json.Object
        -> Json.Parser (Set (Ledger.Credential 'Staking crypto))
    decodeCredentials obj = fmap fromList $
        obj .: "delegationsAndRewards" >>= traverse decodeCredential

parseGetCurrentPParams
    :: forall crypto f. (Typeable crypto)
    => (forall era. Typeable era => Proxy era -> GenResult crypto f (Ledger.PParams era))
    -> Json.Value
    -> Json.Parser (QueryInEra f (CardanoBlock crypto))
parseGetCurrentPParams genResultInEra =
    Json.withText "SomeQuery" $ \text -> do
        guard (text == "currentProtocolParameters") $> \case
            SomeSophieEra SophieBasedEraSophie ->
                Just $ SomeQuery
                { query =
                    LSQ.BlockQuery $ QueryIfCurrentSophie GetCurrentPParams
                , encodeResult =
                    const (either encodeMismatchEraInfo (Sophie.encodePParams' id))
                , genResult =
                    genResultInEra (Proxy @(SophieEra crypto))
                }
            SomeSophieEra SophieBasedEraAllegra ->
                Just $ SomeQuery
                { query =
                    LSQ.BlockQuery $ QueryIfCurrentAllegra GetCurrentPParams
                , encodeResult =
                    const (either encodeMismatchEraInfo (Evie.encodePParams' id))
                , genResult =
                    genResultInEra (Proxy @(AllegraEra crypto))
                }
            SomeSophieEra SophieBasedEraJen ->
                Just $ SomeQuery
                { query =
                    LSQ.BlockQuery $ QueryIfCurrentJen GetCurrentPParams
                , encodeResult =
                    const (either encodeMismatchEraInfo (Jen.encodePParams' id))
                , genResult =
                    genResultInEra (Proxy @(JenEra crypto))
                }
            SomeSophieEra SophieBasedEraAurum ->
                Just $ SomeQuery
                { query =
                    LSQ.BlockQuery $ QueryIfCurrentAurum GetCurrentPParams
                , encodeResult =
                    const (either encodeMismatchEraInfo (Aurum.encodePParams' id))
                , genResult =
                    genResultInEra (Proxy @(AurumEra crypto))
                }

parseGetProposedPParamsUpdates
    :: forall crypto f. (Crypto crypto)
    => (forall era. Typeable era => Proxy era -> GenResult crypto f (Sh.ProposedPPUpdates era))
    -> Json.Value
    -> Json.Parser (QueryInEra f (CardanoBlock crypto))
parseGetProposedPParamsUpdates genResultInEra =
    Json.withText "SomeQuery" $ \text -> do
        guard (text == "proposedProtocolParameters") $> \case
            SomeSophieEra SophieBasedEraSophie ->
                Just $ SomeQuery
                { query =
                    LSQ.BlockQuery $ QueryIfCurrentSophie GetProposedPParamsUpdates
                , encodeResult =
                    const (either encodeMismatchEraInfo Sophie.encodeProposedPPUpdates)
                , genResult =
                    genResultInEra (Proxy @(SophieEra crypto))
                }
            SomeSophieEra SophieBasedEraAllegra ->
                Just $ SomeQuery
                { query =
                    LSQ.BlockQuery $ QueryIfCurrentAllegra GetProposedPParamsUpdates
                , encodeResult =
                    const (either encodeMismatchEraInfo Evie.encodeProposedPPUpdates)
                , genResult =
                    genResultInEra (Proxy @(AllegraEra crypto))
                }
            SomeSophieEra SophieBasedEraJen ->
                Just $ SomeQuery
                { query =
                    LSQ.BlockQuery $ QueryIfCurrentJen GetProposedPParamsUpdates
                , encodeResult =
                    const (either encodeMismatchEraInfo Jen.encodeProposedPPUpdates)
                , genResult =
                    genResultInEra (Proxy @(JenEra crypto))
                }
            SomeSophieEra SophieBasedEraAurum ->
                Just $ SomeQuery
                { query =
                    LSQ.BlockQuery $ QueryIfCurrentAurum GetProposedPParamsUpdates
                , encodeResult =
                    const (either encodeMismatchEraInfo Aurum.encodeProposedPPUpdates)
                , genResult =
                    genResultInEra (Proxy @(AurumEra crypto))
                }

parseGetStakeDistribution
    :: forall crypto f. (Crypto crypto)
    => GenResult crypto f (Ledger.PoolDistr crypto)
    -> Json.Value
    -> Json.Parser (QueryInEra f (CardanoBlock crypto))
parseGetStakeDistribution genResult =
    Json.withText "SomeQuery" $ \text -> do
        guard (text == "stakeDistribution") $>
            ( \query -> Just $ SomeQuery
                { query
                , genResult
                , encodeResult =
                    either encodeMismatchEraInfo . encodePoolDistr
                }
            )
            .
            ( \case
                SomeSophieEra SophieBasedEraSophie ->
                    LSQ.BlockQuery $ QueryIfCurrentSophie GetStakeDistribution
                SomeSophieEra SophieBasedEraAllegra ->
                    LSQ.BlockQuery $ QueryIfCurrentAllegra GetStakeDistribution
                SomeSophieEra SophieBasedEraJen ->
                    LSQ.BlockQuery $ QueryIfCurrentJen GetStakeDistribution
                SomeSophieEra SophieBasedEraAurum ->
                    LSQ.BlockQuery $ QueryIfCurrentAurum GetStakeDistribution
            )

parseGetSystemStart
    :: forall crypto f. ()
    => (Proxy SystemStart -> f SystemStart)
    -> Json.Value
    -> Json.Parser (QueryInEra f (CardanoBlock crypto))
parseGetSystemStart genResult =
    Json.withText "SomeQuery" $ \text -> do
        guard (text == "systemStart")
        pure $ const $ Just $ SomeQuery
            { query = LSQ.GetSystemStart
            , genResult
            , encodeResult = const encodeSystemStart
            }

parseGetUTxO
    :: forall crypto f. (Crypto crypto)
    => (forall era. Typeable era => Proxy era -> GenResult crypto f (Sh.UTxO era))
    -> Json.Value
    -> Json.Parser (QueryInEra f (CardanoBlock crypto))
parseGetUTxO genResultInEra =
    Json.withText "SomeQuery" $ \text -> do
        guard (text == "utxo") $> \case
            SomeSophieEra SophieBasedEraSophie ->
                Just $ SomeQuery
                { query =
                    LSQ.BlockQuery $ QueryIfCurrentSophie GetUTxOWhole
                , encodeResult =
                    either encodeMismatchEraInfo . Sophie.encodeUtxoWithMode
                , genResult =
                    genResultInEra (Proxy @(SophieEra crypto))
                }
            SomeSophieEra SophieBasedEraAllegra ->
                Just $ SomeQuery
                { query =
                    LSQ.BlockQuery $ QueryIfCurrentAllegra GetUTxOWhole
                , encodeResult =
                    either encodeMismatchEraInfo . Evie.encodeUtxoWithMode
                , genResult =
                    genResultInEra (Proxy @(AllegraEra crypto))
                }
            SomeSophieEra SophieBasedEraJen ->
                Just $ SomeQuery
                { query =
                    LSQ.BlockQuery $ QueryIfCurrentJen GetUTxOWhole
                , encodeResult =
                    either encodeMismatchEraInfo . Jen.encodeUtxoWithMode
                , genResult =
                    genResultInEra (Proxy @(JenEra crypto))
                }
            SomeSophieEra SophieBasedEraAurum ->
                Just $ SomeQuery
                { query =
                    LSQ.BlockQuery $ QueryIfCurrentAurum GetUTxOWhole
                , encodeResult =
                    either encodeMismatchEraInfo . Aurum.encodeUtxoWithMode
                , genResult =
                    genResultInEra (Proxy @(AurumEra crypto))
                }

parseGetUTxOByAddress
    :: forall crypto f. (Crypto crypto)
    => (forall era. Typeable era => Proxy era -> GenResult crypto f (Sh.UTxO era))
    -> Json.Value
    -> Json.Parser (QueryInEra f (CardanoBlock crypto))
parseGetUTxOByAddress genResultInEra = Json.withObject "SomeQuery" $ \obj -> do
    addrs <- decodeAddresses obj
    pure $ \case
        SomeSophieEra SophieBasedEraSophie ->
            Just $ SomeQuery
            { query =
                LSQ.BlockQuery $ QueryIfCurrentSophie (GetUTxOByAddress addrs)
            , encodeResult =
                either encodeMismatchEraInfo . Sophie.encodeUtxoWithMode
            , genResult =
                genResultInEra (Proxy @(SophieEra crypto))
            }
        SomeSophieEra SophieBasedEraAllegra ->
            Just $ SomeQuery
            { query =
                LSQ.BlockQuery $ QueryIfCurrentAllegra (GetUTxOByAddress addrs)
            , encodeResult =
                either encodeMismatchEraInfo . Evie.encodeUtxoWithMode
            , genResult =
                genResultInEra (Proxy @(AllegraEra crypto))
            }
        SomeSophieEra SophieBasedEraJen ->
            Just $ SomeQuery
            { query =
                LSQ.BlockQuery $ QueryIfCurrentJen (GetUTxOByAddress addrs)
            , encodeResult =
                either encodeMismatchEraInfo . Jen.encodeUtxoWithMode
            , genResult =
                genResultInEra (Proxy @(JenEra crypto))
            }
        SomeSophieEra SophieBasedEraAurum ->
            Just $ SomeQuery
            { query =
                LSQ.BlockQuery $ QueryIfCurrentAurum (GetUTxOByAddress addrs)
            , encodeResult =
                either encodeMismatchEraInfo . Aurum.encodeUtxoWithMode
            , genResult =
                genResultInEra (Proxy @(AurumEra crypto))
            }
  where
    decodeAddresses
        :: Json.Object
        -> Json.Parser (Set (Ledger.Addr crypto))
    decodeAddresses obj = fmap fromList $
        obj .: "utxo" >>= traverse decodeAddress

parseGetUTxOByTxIn
    :: forall crypto f. (Crypto crypto)
    => (forall era. Typeable era => Proxy era -> GenResult crypto f (Sh.UTxO era))
    -> Json.Value
    -> Json.Parser (QueryInEra f (CardanoBlock crypto))
parseGetUTxOByTxIn genResultInEra = Json.withObject "SomeQuery" $ \obj -> do
    ins <- decodeTxIns obj
    pure $ \case
        SomeSophieEra SophieBasedEraSophie ->
            Just $ SomeQuery
            { query =
                LSQ.BlockQuery $ QueryIfCurrentSophie (GetUTxOByTxIn ins)
            , encodeResult =
                either encodeMismatchEraInfo . Sophie.encodeUtxoWithMode
            , genResult =
                genResultInEra (Proxy @(SophieEra crypto))
            }
        SomeSophieEra SophieBasedEraAllegra ->
            Just $ SomeQuery
            { query =
                LSQ.BlockQuery $ QueryIfCurrentAllegra (GetUTxOByTxIn ins)
            , encodeResult =
                either encodeMismatchEraInfo . Evie.encodeUtxoWithMode
            , genResult =
                genResultInEra (Proxy @(AllegraEra crypto))
            }
        SomeSophieEra SophieBasedEraJen ->
            Just $ SomeQuery
            { query =
                LSQ.BlockQuery $ QueryIfCurrentJen (GetUTxOByTxIn ins)
            , encodeResult =
                either encodeMismatchEraInfo . Jen.encodeUtxoWithMode
            , genResult =
                genResultInEra (Proxy @(JenEra crypto))
            }
        SomeSophieEra SophieBasedEraAurum ->
            Just $ SomeQuery
            { query =
                LSQ.BlockQuery $ QueryIfCurrentAurum (GetUTxOByTxIn ins)
            , encodeResult =
                either encodeMismatchEraInfo . Aurum.encodeUtxoWithMode
            , genResult =
                genResultInEra (Proxy @(AurumEra crypto))
            }
  where
    decodeTxIns
        :: Json.Object
        -> Json.Parser (Set (Ledger.TxIn crypto))
    decodeTxIns obj = fmap fromList $
        obj .: "utxo" >>= traverse decodeTxIn

-- TODO: This query seems to actually always return a compact version of the
-- `SophieGenesis`, even when queried from Aurum. While this is practical
-- (because the return type does not change when crossing eras), there's also no
-- way currently to retrieve an `AurumGenesis` ¯\_(ツ)_/¯
--
-- If this query is indeed meant to only return SophieGenesis, renaming it to
-- something which suggests it better would make sense. I've asked *the guys*.
parseGetGenesisConfig
    :: forall f crypto. (Crypto crypto)
    => (forall era. Typeable era => Proxy era -> GenResult crypto f (CompactGenesis era))
    -> Json.Value
    -> Json.Parser (QueryInEra f (CardanoBlock crypto))
parseGetGenesisConfig genResultInEra = do
    Json.withText "SomeQuery" $ \text -> do
        guard (text == "genesisConfig") $> \case
            SomeSophieEra SophieBasedEraSophie ->
                Just $ SomeQuery
                { query =
                    LSQ.BlockQuery $ QueryIfCurrentSophie GetGenesisConfig
                , encodeResult =
                    let encodeGenesis =
                            Sophie.encodeGenesis . getCompactGenesis
                    in const (either encodeMismatchEraInfo encodeGenesis)
                , genResult =
                    genResultInEra (Proxy @(SophieEra crypto))
                }
            SomeSophieEra SophieBasedEraAllegra ->
                Just $ SomeQuery
                { query =
                    LSQ.BlockQuery $ QueryIfCurrentAllegra GetGenesisConfig
                , encodeResult =
                    let encodeGenesis =
                            Sophie.encodeGenesis . getCompactGenesis
                    in const (either encodeMismatchEraInfo encodeGenesis)
                , genResult =
                    genResultInEra (Proxy @(AllegraEra crypto))
                }
            SomeSophieEra SophieBasedEraJen ->
                Just $ SomeQuery
                { query =
                    LSQ.BlockQuery $ QueryIfCurrentJen GetGenesisConfig
                , encodeResult =
                    let encodeGenesis =
                            Sophie.encodeGenesis . getCompactGenesis
                    in const (either encodeMismatchEraInfo encodeGenesis)
                , genResult =
                    genResultInEra (Proxy @(JenEra crypto))
                }
            SomeSophieEra SophieBasedEraAurum ->
                Just $ SomeQuery
                { query =
                    LSQ.BlockQuery $ QueryIfCurrentAurum GetGenesisConfig
                , encodeResult =
                    let encodeGenesis =
                            Sophie.encodeGenesis . getCompactGenesis
                     in const (either encodeMismatchEraInfo encodeGenesis)
                , genResult =
                    genResultInEra (Proxy @(AurumEra crypto))
                }

parseGetRewardProvenance
    :: forall crypto f. (Crypto crypto)
    => GenResult crypto f (Sh.RewardProvenance crypto)
    -> Json.Value
    -> Json.Parser (QueryInEra f (CardanoBlock crypto))
parseGetRewardProvenance genResult =
    Json.withText "SomeQuery" $ \text -> do
        guard (text == "rewardsProvenance") $>
            ( \query -> Just $ SomeQuery
                { query
                , genResult
                , encodeResult =
                    either encodeMismatchEraInfo . encodeRewardProvenance
                }
            )
            .
            ( \case
                SomeSophieEra SophieBasedEraSophie ->
                    LSQ.BlockQuery $ QueryIfCurrentSophie GetRewardProvenance
                SomeSophieEra SophieBasedEraAllegra ->
                    LSQ.BlockQuery $ QueryIfCurrentAllegra GetRewardProvenance
                SomeSophieEra SophieBasedEraJen ->
                    LSQ.BlockQuery $ QueryIfCurrentJen GetRewardProvenance
                SomeSophieEra SophieBasedEraAurum ->
                    LSQ.BlockQuery $ QueryIfCurrentAurum GetRewardProvenance
            )

parseGetRewardInfoPools
    :: forall crypto f. (Crypto crypto)
    => GenResult crypto f (RewardProvenance' crypto)
    -> Json.Value
    -> Json.Parser (QueryInEra f (CardanoBlock crypto))
parseGetRewardInfoPools genResult =
    Json.withText "SomeQuery" $ \text -> do
        guard (text == "rewardsProvenance'") $>
            ( \query -> Just $ SomeQuery
                { query
                , genResult
                , encodeResult = \_ ->
                    either encodeMismatchEraInfo encodeRewardInfoPools
                }
            )
            .
            ( \case
                SomeSophieEra SophieBasedEraSophie ->
                    LSQ.BlockQuery $ QueryIfCurrentSophie GetRewardInfoPools
                SomeSophieEra SophieBasedEraAllegra ->
                    LSQ.BlockQuery $ QueryIfCurrentAllegra GetRewardInfoPools
                SomeSophieEra SophieBasedEraJen ->
                    LSQ.BlockQuery $ QueryIfCurrentJen GetRewardInfoPools
                SomeSophieEra SophieBasedEraAurum ->
                    LSQ.BlockQuery $ QueryIfCurrentAurum GetRewardInfoPools
            )

parseGetPoolIds
    :: forall crypto f. (Crypto crypto)
    => GenResult crypto f (Set (Ledger.KeyHash 'StakePool crypto))
    -> Json.Value
    -> Json.Parser (QueryInEra f (CardanoBlock crypto))
parseGetPoolIds genResult =
    Json.withText "SomeQuery" $ \text -> do
        guard (text == "poolIds") $>
            ( \query -> Just $ SomeQuery
                { query
                , genResult
                , encodeResult = \mode ->
                    either encodeMismatchEraInfo (encodeListWithMode mode Sophie.encodePoolId . toList)
                }
            )
            .
            ( \case
                SomeSophieEra SophieBasedEraSophie ->
                    LSQ.BlockQuery $ QueryIfCurrentSophie GetStakePools
                SomeSophieEra SophieBasedEraAllegra ->
                    LSQ.BlockQuery $ QueryIfCurrentAllegra GetStakePools
                SomeSophieEra SophieBasedEraJen ->
                    LSQ.BlockQuery $ QueryIfCurrentJen GetStakePools
                SomeSophieEra SophieBasedEraAurum ->
                    LSQ.BlockQuery $ QueryIfCurrentAurum GetStakePools
            )

parseGetPoolParameters
    :: forall crypto f. (Crypto crypto)
    => GenResult crypto f (Map (Ledger.KeyHash 'StakePool crypto) (Sh.PoolParams crypto))
    -> Json.Value
    -> Json.Parser (QueryInEra f (CardanoBlock crypto))
parseGetPoolParameters genResult =
    Json.withObject "SomeQuery" $ \obj -> do
        ids <- decodePoolIds obj
        pure $
            (\query -> Just $ SomeQuery
                { query
                , genResult
                , encodeResult =
                    either encodeMismatchEraInfo . encodePoolParameters
                }
            )
            .
            ( \case
                SomeSophieEra SophieBasedEraSophie ->
                    LSQ.BlockQuery $ QueryIfCurrentSophie (GetStakePoolParams ids)
                SomeSophieEra SophieBasedEraAllegra ->
                    LSQ.BlockQuery $ QueryIfCurrentAllegra (GetStakePoolParams ids)
                SomeSophieEra SophieBasedEraJen ->
                    LSQ.BlockQuery $ QueryIfCurrentJen (GetStakePoolParams ids)
                SomeSophieEra SophieBasedEraAurum ->
                    LSQ.BlockQuery $ QueryIfCurrentAurum (GetStakePoolParams ids)
            )
  where
    decodePoolIds
        :: Json.Object
        -> Json.Parser (Set (Ledger.KeyHash 'StakePool crypto))
    decodePoolIds obj = fmap fromList $
        obj .: "poolParameters" >>= traverse decodePoolId

parseGetPoolsRanking
    :: forall crypto f. (Crypto crypto)
    => GenResult crypto f (Sh.RewardProvenance crypto)
    -> Json.Value
    -> Json.Parser (QueryInEra f (CardanoBlock crypto))
parseGetPoolsRanking genResult =
    Json.withText "SomeQuery" $ \text -> do
        guard (text == "poolsRanking") $>
            ( \query -> Just $ SomeQuery
                { query
                , genResult
                , encodeResult =
                    either encodeMismatchEraInfo . encodeDesirabilities
                }
            )
            .
            ( \case
                SomeSophieEra SophieBasedEraSophie ->
                    LSQ.BlockQuery $ QueryIfCurrentSophie GetRewardProvenance
                SomeSophieEra SophieBasedEraAllegra ->
                    LSQ.BlockQuery $ QueryIfCurrentAllegra GetRewardProvenance
                SomeSophieEra SophieBasedEraJen ->
                    LSQ.BlockQuery $ QueryIfCurrentJen GetRewardProvenance
                SomeSophieEra SophieBasedEraAurum ->
                    LSQ.BlockQuery $ QueryIfCurrentAurum GetRewardProvenance
            )

parseGetInterpreter
    :: forall crypto f. ()
    => (Proxy (Interpreter (CardanoEras crypto)) -> f (Interpreter (CardanoEras crypto)))
    -> Json.Value
    -> Json.Parser (QueryInEra f (CardanoBlock crypto))
parseGetInterpreter genResult =
    Json.withText "SomeQuery" $ \text -> do
        guard (text == "eraSummaries")
        pure $ const $ Just $ SomeQuery
            { query = LSQ.BlockQuery $ LSQ.QueryHardFork LSQ.GetInterpreter
            , genResult
            , encodeResult = const encodeInterpreter
            }

--
-- Parsers (Others)
--

decodeAddress
    :: Crypto crypto
    => Json.Value
    -> Json.Parser (Ledger.Addr crypto)
decodeAddress = Json.withText "Address" $ choice "address"
    [ addressFromBytes fromBech32
    , addressFromBytes fromBase58
    , addressFromBytes fromBase16
    ]
  where
    addressFromBytes decode =
        decode >=> maybe mempty pure . Ledger.deserialiseAddr

    fromBech32 txt =
        case Bech32.decodeLenient txt of
            Left e ->
                fail (show e)
            Right (_, dataPart) ->
                maybe mempty pure $ Bech32.dataPartToBytes dataPart

    fromBase58 =
        decodeBase58 . encodeUtf8

    fromBase16 =
        decodeBase16 . encodeUtf8

decodeAssets
    :: Crypto crypto
    => Json.Value
    -> Json.Parser [(Ledger.Jen.PolicyID crypto, Ledger.Jen.AssetName, Integer)]
decodeAssets =
    Json.withObject "Assets" $ HMap.foldrWithKey' fn (pure mempty)
  where
    fn k v p = do
        xs <- p
        (policyId, assetName) <- decodeAssetId k
        quantity <- Json.parseJSON v
        pure $ (policyId, assetName, quantity) : xs

decodeAssetId
    :: Crypto crypto
    => Text
    -> Json.Parser (Ledger.Jen.PolicyID crypto, Ledger.Jen.AssetName)
decodeAssetId txt =
    case T.splitOn "." txt of
        [rawPolicyId] ->
            let emptyAssetName = Ledger.Jen.AssetName mempty in
            (,) <$> decodePolicyId rawPolicyId <*> pure emptyAssetName
        [rawPolicyId, rawAssetName] ->
            (,) <$> decodePolicyId rawPolicyId <*> decodeAssetName rawAssetName
        _ ->
            fail "invalid asset id, should be a dot-separated policy id and asset name, both base16-encoded."

decodeAssetName
    :: Text
    -> Json.Parser Ledger.Jen.AssetName
decodeAssetName =
    fmap Ledger.Jen.AssetName . decodeBase16 . encodeUtf8

decodeCoin
    :: Json.Value
    -> Json.Parser Coin
decodeCoin =
    fmap Ledger.word64ToCoin . Json.parseJSON

-- TODO: Makes it possible to distinguish between KeyHash and ScriptHash
-- credentials. Both are encoded as hex-encoded strings. Encoding them as
-- object is ill-advised because we also need them as key of the non-myopic
-- member rewards map.
--
-- A possible option: encode them as Bech32 strings with different prefixes.
decodeCredential
    :: Crypto crypto
    => Json.Value
    -> Json.Parser (Ledger.Credential 'Staking crypto)
decodeCredential =
    fmap (Ledger.KeyHashObj . Ledger.KeyHash) . decodeHash

decodeDatumHash
    :: Crypto crypto
    => Json.Value
    -> Json.Parser (Ledger.Aurum.DataHash crypto)
decodeDatumHash =
    fmap unsafeMakeSafeHash . decodeHash

decodeHash
    :: CC.HashAlgorithm alg
    => Json.Value
    -> Json.Parser (CC.Hash alg a)
decodeHash =
    Json.parseJSON >=> maybe empty pure . CC.hashFromTextAsHex

decodePolicyId
    :: Crypto crypto
    => Text
    -> Json.Parser (Ledger.Jen.PolicyID crypto)
decodePolicyId =
    maybe
        invalidPolicyId
        (pure . Ledger.Jen.PolicyID . Ledger.ScriptHash)
    . CC.hashFromTextAsHex
  where
    invalidPolicyId = fail "failed to decode policy id for a given asset."

decodePoolId
    :: Crypto crypto
    => Json.Value
    -> Json.Parser (Ledger.KeyHash 'StakePool crypto)
decodePoolId = Json.withText "PoolId" $ choice "poolId"
    [ poolIdFromBytes fromBech32
    , poolIdFromBytes fromBase16
    ]
  where
    poolIdFromBytes decode =
        decode >=> maybe empty (pure . Ledger.KeyHash) . hashFromBytes

    fromBech32 txt =
        case Bech32.decodeLenient txt of
            Left e ->
                fail (show e)
            Right (_, dataPart) ->
                maybe empty pure $ Bech32.dataPartToBytes dataPart

    fromBase16 =
        decodeBase16 . encodeUtf8

decodeTxIn
    :: forall crypto. (Crypto crypto)
    => Json.Value
    -> Json.Parser (Ledger.TxIn crypto)
decodeTxIn = Json.withObject "TxIn" $ \o -> do
    txid <- o .: "txId" >>= fromBase16
    ix <- o .: "index"
    pure $ Ledger.TxIn (Ledger.TxId txid) ix
  where
    fromBase16 =
        maybe empty (pure . unsafeMakeSafeHash) . hashFromTextAsHex @(HASH crypto)

decodeTxOut
    :: forall crypto. (Crypto crypto)
    => Json.Value
    -> Json.Parser (Ledger.TxOut (AurumEra crypto))
decodeTxOut = Json.withObject "TxOut" $ \o -> do
    address <- o .: "address" >>= decodeAddress
    value <- o .: "value" >>= decodeValue
    datum <- o .:? "datum" >>= maybe (pure SNothing) (fmap SJust . decodeDatumHash)
    pure (Ledger.Aurum.TxOut address value datum)

decodeValue
    :: forall crypto. (Crypto crypto)
    => Json.Value
    -> Json.Parser (Ledger.Value (AurumEra crypto))
decodeValue = Json.withObject "Value" $ \o -> do
    coins <- o .: "coins" >>= decodeCoin
    assets <- o .:? "assets" >>= maybe mempty decodeAssets
    pure (Ledger.Jen.valueFromList (Ledger.unCoin coins) assets)

--
-- Helpers
--

-- NOTE: This is necessary because the constructor of 'Hash' is no longer
-- exposed, and thus, it is not possible to use the 'castPoint' function from
-- Shardagnostic.Network.Block anymore! May revisit in future upgrade of the
-- dependencies.
castPoint
    :: forall era crypto. (Ledger.Crypto era ~ crypto, Crypto crypto)
    => Point (SophieBlock era)
    -> Point (CardanoBlock crypto)
castPoint = \case
    GenesisPoint -> GenesisPoint
    BlockPoint slot h -> BlockPoint slot (cast h)
  where
    cast (TOptimum.unHashHeader . unSophieHash -> UnsafeHash h) = coerce h
