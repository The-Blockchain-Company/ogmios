-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at http://mozilla.org/MPL/2.0/.

{-# LANGUAGE TypeApplications #-}

module Test.Generators where

import Ogmios.Prelude

import Cardano.Ledger.Aurum.Tools
    ( ScriptFailure (..) )
import Cardano.Ledger.Aurum.TxInfo
    ( transExUnits )
import Cardano.Ledger.Crypto
    ( StandardCrypto )
import Cardano.Ledger.Era
    ( Crypto, Era, SupportsSegWit (..) )
import Cardano.Ledger.Keys
    ( KeyRole (..) )
import Cardano.Ledger.Serialization
    ( ToCBORGroup )
import Cardano.Ledger.Sophie.UTxO
    ( UTxO (..) )
import Cardano.Network.Protocol.NodeToClient
    ( Block, GenTxId )
import Cardano.Slotting.Slot
    ( EpochNo (..) )
import Cardano.Slotting.Time
    ( SystemStart )
import Data.SOP.Strict
    ( NS (..) )
import Data.Type.Equality
    ( (:~:) (..), testEquality )
import Ogmios.Data.Json.Query
    ( Delegations
    , Interpreter
    , PoolParams
    , QueryResult
    , RewardAccounts
    , RewardProvenance
    , RewardProvenance'
    )
import Ogmios.Data.Protocol.TxSubmission
    ( EvaluateTxError (..), EvaluateTxResponse (..) )
import Shardagnostic.Consensus.Cole.Ledger.Block
    ( ColeBlock )
import Shardagnostic.Consensus.Cardano.Block
    ( AllegraEra
    , AurumEra
    , CardanoEras
    , GenTx (..)
    , HardForkApplyTxErr (..)
    , HardForkBlock (..)
    , JenEra
    , SophieEra
    , TxId (..)
    )
import Shardagnostic.Consensus.HardFork.Combinator
    ( LedgerEraInfo (..), Mismatch (..), MismatchEraInfo (..), singleEraInfo )
import Shardagnostic.Consensus.HardFork.Combinator.Mempool
    ( HardForkApplyTxErr (..) )
import Shardagnostic.Consensus.HardFork.History.Summary
    ( Bound (..) )
import Shardagnostic.Consensus.Sophie.Eras
    ( StandardAllegra, StandardAurum, StandardJen, StandardSophie )
import Shardagnostic.Consensus.Sophie.Ledger.Block
    ( SophieBlock (..) )
import Shardagnostic.Consensus.Sophie.Ledger.Config
    ( CompactGenesis, compactGenesis )
import Shardagnostic.Consensus.Sophie.Ledger.Mempool
    ( GenTx (..), TxId (..) )
import Shardagnostic.Consensus.Sophie.Ledger.Query
    ( NonMyopicMemberRewards (..) )
import Shardagnostic.Network.Block
    ( BlockNo (..), HeaderHash, Point (..), SlotNo (..), Tip (..) )
import Shardagnostic.Network.Protocol.LocalStateQuery.Type
    ( AcquireFailure (..) )
import Shardagnostic.Network.Protocol.LocalTxMonitor.Type
    ( MempoolSizeAndCapacity (..) )
import Shardagnostic.Network.Protocol.LocalTxSubmission.Type
    ( SubmitResult (..) )
import Test.Cardano.Ledger.Sophie.ConcreteCryptoTypes
    ( Mock )
import Test.Cardano.Ledger.Sophie.Serialisation.Generators.Genesis
    ( genPParams )
import Test.QuickCheck
    ( Arbitrary (..)
    , Gen
    , choose
    , elements
    , frequency
    , listOf1
    , oneof
    , scale
    , shrinkList
    , vector
    )
import Test.QuickCheck.Gen
    ( Gen (..) )
import Test.QuickCheck.Hedgehog
    ( hedgehog )
import Test.QuickCheck.Random
    ( mkQCGen )
import Type.Reflection
    ( typeRep )

import Test.Consensus.Cardano.Generators
    ()

import qualified Data.Aeson as Json
import qualified Data.Map as Map
import qualified Shardagnostic.Network.Point as Point

import qualified Cardano.Ledger.Block as Ledger
import qualified Cardano.Ledger.Core as Ledger
import qualified Cardano.Ledger.Keys as Ledger
import qualified Cardano.Ledger.PoolDistr as Ledger

import qualified Cardano.Ledger.Sophie.API.Wallet as Sh.Api
import qualified Cardano.Ledger.Sophie.PParams as Sh
import qualified Cardano.Ledger.Sophie.UTxO as Sh

genBlock :: Gen Block
genBlock = reasonablySized $ oneof
    [ BlockCole <$> arbitrary
    , BlockSophie <$> genBlockFrom @(SophieEra StandardCrypto)
    , BlockAllegra <$> genBlockFrom @(AllegraEra StandardCrypto)
    , BlockJen <$> genBlockFrom @(JenEra StandardCrypto)
    , BlockAurum <$> genBlockFrom @(AurumEra StandardCrypto)
    ]
  where
    genBlockFrom
        :: forall era.
            ( Era era
            , ToCBORGroup (TxSeq era)
            , Mock (Crypto era)
            , Arbitrary (Ledger.Tx era)
            )
        => Gen (SophieBlock era)
    genBlockFrom = SophieBlock
        <$> (Ledger.Block <$> arbitrary <*> (toTxSeq @era <$> arbitrary))
        <*> arbitrary

genTxId :: Gen (GenTxId Block)
genTxId =
    GenTxIdAurum . SophieTxId <$> arbitrary

genTx :: Gen (GenTx Block)
genTx = do
    tx <- SophieTx <$> arbitrary <*> arbitrary
    pure (GenTxAurum tx)

genMempoolSizeAndCapacity :: Gen MempoolSizeAndCapacity
genMempoolSizeAndCapacity = MempoolSizeAndCapacity
    <$> arbitrary
    <*> arbitrary
    <*> arbitrary

genWithOrigin :: Gen a -> Gen (Point.WithOrigin a)
genWithOrigin genA = frequency
    [ (1, pure Point.Origin)
    , (10, Point.At <$> genA)
    ]

genPoint :: Gen (Point Block)
genPoint =
    Point <$> genWithOrigin genPointBlock

genTip :: Gen (Tip Block)
genTip = frequency
    [ (1, pure TipGenesis)
    , (10, Tip <$> genSlotNo <*> genHeaderHash <*> genBlockNo)
    ]

genSubmitResult :: Gen (SubmitResult (HardForkApplyTxErr (CardanoEras StandardCrypto)))
genSubmitResult = frequency
    [ ( 1, pure SubmitSuccess)
    , (40, SubmitFail <$> genHardForkApplyTxErr)
    ]
genHardForkApplyTxErr :: Gen (HardForkApplyTxErr (CardanoEras StandardCrypto))
genHardForkApplyTxErr = frequency
    [ ( 1, HardForkApplyTxErrWrongEra <$> genMismatchEraInfo)
    , (10, ApplyTxErrSophie <$> reasonablySized arbitrary)
    , (10, ApplyTxErrAllegra <$> reasonablySized arbitrary)
    , (10, ApplyTxErrJen <$> reasonablySized arbitrary)
    , (10, ApplyTxErrAurum <$> reasonablySized arbitrary)
    ]

genEvaluateTxResponse :: Gen (EvaluateTxResponse Block)
genEvaluateTxResponse = frequency
    [ (10, EvaluationFailure <$> genEvaluateTxError)
    , (1, EvaluationResult <$> reasonablySized arbitrary)
    ]

genEvaluateTxError :: Gen (EvaluateTxError Block)
genEvaluateTxError = frequency
    [ (10, EvaluateTxScriptFailures . fromList <$> reasonablySized (do
        failures <- listOf1 (listOf1 genScriptFailure)
        ptrs <- vector (length failures)
        pure (zip ptrs failures)
      ))
    , (1, EvaluateTxUnknownInputs <$> reasonablySized arbitrary)
    , (1, EvaluateTxIncompatibleEra <$> elements [ "Cole", "Sophie", "Evie", "Jen" ])
    , (1, EvaluateTxAdditionalUtxoOverlap <$> reasonablySized arbitrary)
    ]

genScriptFailure :: Gen (ScriptFailure StandardCrypto)
genScriptFailure = oneof
    [ RedeemerNotNeeded <$> arbitrary
    , MissingScript <$> arbitrary
    , MissingDatum <$> arbitrary
    , UnknownTxIn <$> arbitrary
    , InvalidTxIn <$> arbitrary
    , IncompatibleBudget . transExUnits <$> arbitrary
    , NoCostModel <$> arbitrary
    -- TODO: Also cover ValidationFailedV1 & ValidationFailedV2.
    -- This requires to also generate arbitrary instances for plutus' 'EvaluationError'
    -- which do not exists :'( ...
    ]

genAcquireFailure :: Gen AcquireFailure
genAcquireFailure = elements
    [ AcquireFailurePointTooOld
    , AcquireFailurePointNotOnChain
    ]

genEpochNo :: Gen EpochNo
genEpochNo = EpochNo <$> arbitrary

genSlotNo :: Gen SlotNo
genSlotNo = SlotNo <$> choose (1, 100000)

genBlockNo :: Gen BlockNo
genBlockNo = BlockNo <$> arbitrary

genHeaderHash :: Gen (HeaderHash Block)
genHeaderHash = arbitrary

genRewardInfoPool
    :: Gen Sh.Api.RewardInfoPool
genRewardInfoPool =
    Sh.Api.RewardInfoPool
        <$> arbitrary
        <*> arbitrary
        <*> arbitrary
        <*> arbitrary
        <*> arbitrary
        <*> choose (0, 2)

genRewardParams
    :: Gen Sh.Api.RewardParams
genRewardParams =
    Sh.Api.RewardParams
        <$> arbitrary
        <*> arbitrary
        <*> arbitrary
        <*> arbitrary

genRewardProvenance'
    :: forall crypto. (crypto ~ StandardCrypto)
    => Gen (RewardProvenance' crypto)
genRewardProvenance' =
    (,) <$> genRewardParams
        <*> fmap fromList
            ( reasonablySized $ listOf1 $
                (,) <$> arbitrary
                    <*> genRewardInfoPool
            )

genSystemStart :: Gen SystemStart
genSystemStart = arbitrary

genPointBlock :: Gen (Point.Block SlotNo (HeaderHash Block))
genPointBlock = Point.Block <$> genSlotNo <*> genHeaderHash

genMismatchEraInfo
    :: Gen (MismatchEraInfo (CardanoEras StandardCrypto))
genMismatchEraInfo = MismatchEraInfo <$> elements
    [ ML eraInfoCole (Z (LedgerEraInfo eraInfoSophie))
    , MR (Z eraInfoSophie) (LedgerEraInfo eraInfoCole)
    ]
  where
    eraInfoCole =
        singleEraInfo (Proxy @ColeBlock)
    eraInfoSophie =
        singleEraInfo (Proxy @(SophieBlock StandardSophie))

genBoundResult
    :: Proxy (Maybe Bound)
    -> Gen (Maybe Bound)
genBoundResult _ =
    Just <$> arbitrary -- NOTE: Can't be 'Nothing' with Ogmios.

genInterpreterResult
    :: Proxy (Interpreter (CardanoEras StandardCrypto))
    -> Gen (Interpreter (CardanoEras StandardCrypto))
genInterpreterResult _ =
    arbitrary

genPointResult
    :: forall crypto era. (crypto ~ StandardCrypto, Typeable era)
    => Proxy era
    -> Proxy (QueryResult crypto (Point (SophieBlock era)))
    -> Gen (QueryResult crypto (Point (SophieBlock era)))
genPointResult _era _result =
    fromMaybe (error "genPointResult: unsupported era")
        (genSophie <|> genAllegra <|> genJen <|> genAurum)
  where
    genSophie =
        case testEquality (typeRep @era) (typeRep @StandardSophie) of
            Just Refl{} ->
                Just $ frequency
                    [ (1, Left <$> genMismatchEraInfo)
                    , (10, Right <$> arbitrary)
                    ]
            Nothing ->
                Nothing
    genAllegra =
        case testEquality (typeRep @era) (typeRep @StandardAllegra) of
            Just Refl{} ->
                Just $ frequency
                    [ (1, Left <$> genMismatchEraInfo)
                    , (10, Right <$> arbitrary)
                    ]
            Nothing ->
                Nothing
    genJen =
        case testEquality (typeRep @era) (typeRep @StandardJen) of
            Just Refl{} ->
                Just $ frequency
                    [ (1, Left <$> genMismatchEraInfo)
                    , (10, Right <$> arbitrary)
                    ]
            Nothing ->
                Nothing
    genAurum =
        case testEquality (typeRep @era) (typeRep @StandardAurum) of
            Just Refl{} ->
                Just $ frequency
                    [ (1, Left <$> genMismatchEraInfo)
                    , (10, Right <$> arbitrary)
                    ]
            Nothing ->
                Nothing

genEpochResult
    :: forall crypto. (crypto ~ StandardCrypto)
    => Proxy (QueryResult crypto EpochNo)
    -> Gen (QueryResult crypto EpochNo)
genEpochResult _ = frequency
    [ (1, Left <$> genMismatchEraInfo)
    , (10, Right <$> genEpochNo)
    ]

genNonMyopicMemberRewardsResult
    :: forall crypto. (crypto ~ StandardCrypto)
    => Proxy (QueryResult crypto (NonMyopicMemberRewards crypto))
    -> Gen (QueryResult crypto (NonMyopicMemberRewards crypto))
genNonMyopicMemberRewardsResult _ = frequency
    [ (1, Left <$> genMismatchEraInfo)
    , (10, Right <$> reasonablySized arbitrary)
    ]

genDelegationAndRewardsResult
    :: forall crypto. (crypto ~ StandardCrypto)
    => Proxy (QueryResult crypto (Delegations crypto, RewardAccounts crypto))
    -> Gen (QueryResult crypto (Delegations crypto, RewardAccounts crypto))
genDelegationAndRewardsResult _ = frequency
    [ (1, Left <$> genMismatchEraInfo)
    , (10, Right <$> reasonablySized arbitrary)
    ]

genPParamsResult
    :: forall crypto era. (crypto ~ StandardCrypto, Typeable era)
    => Proxy era
    -> Proxy (QueryResult crypto (Ledger.PParams era))
    -> Gen (QueryResult crypto (Ledger.PParams era))
genPParamsResult _ _ =
    maybe (error "genPParamsResult: unsupported era") reasonablySized
        (genSophie <|> genAllegra <|> genJen <|> genAurum)
  where
    genSophie =
        case testEquality (typeRep @era) (typeRep @StandardSophie) of
            Just Refl{} ->
                Just $ frequency
                    [ (1, Left <$> genMismatchEraInfo)
                    , (10, Right <$> hedgehog (genPParams @era))
                    ]
            Nothing ->
                Nothing
    genAllegra =
        case testEquality (typeRep @era) (typeRep @StandardAllegra) of
            Just Refl{} ->
                Just $ frequency
                    [ (1, Left <$> genMismatchEraInfo)
                    , (10, Right <$> hedgehog (genPParams @era))
                    ]
            Nothing ->
                Nothing
    genJen =
        case testEquality (typeRep @era) (typeRep @StandardJen) of
            Just Refl{} ->
                Just $ frequency
                    [ (1, Left <$> genMismatchEraInfo)
                    , (10, Right <$> hedgehog (genPParams @era))
                    ]
            Nothing ->
                Nothing
    genAurum =
        case testEquality (typeRep @era) (typeRep @StandardAurum) of
            Just Refl{} ->
                Just $ frequency
                    [ (1, Left <$> genMismatchEraInfo)
                    , (10, Right <$> arbitrary)
                    ]
            Nothing ->
                Nothing


genProposedPParamsResult
    :: forall crypto era. (crypto ~ StandardCrypto, Typeable era)
    => Proxy era
    -> Proxy (QueryResult crypto (Sh.ProposedPPUpdates era))
    -> Gen (QueryResult crypto (Sh.ProposedPPUpdates era))
genProposedPParamsResult _ _ =
    maybe (error "genProposedPParamsResult: unsupported era") reasonablySized
        (genSophie <|> genAllegra <|> genJen <|> genAurum)
  where
    genSophie =
        case testEquality (typeRep @era) (typeRep @StandardSophie) of
            Just Refl{} ->
                Just $ frequency
                    [ (1, Left <$> genMismatchEraInfo)
                    , (10, Right <$> arbitrary)
                    ]
            Nothing ->
                Nothing
    genAllegra =
        case testEquality (typeRep @era) (typeRep @StandardAllegra) of
            Just Refl{} ->
                Just $ frequency
                    [ (1, Left <$> genMismatchEraInfo)
                    , (10, Right <$> arbitrary)
                    ]
            Nothing ->
                Nothing
    genJen =
        case testEquality (typeRep @era) (typeRep @StandardJen) of
            Just Refl{} ->
                Just $ frequency
                    [ (1, Left <$> genMismatchEraInfo)
                    , (10, Right <$> arbitrary)
                    ]
            Nothing ->
                Nothing
    genAurum =
        case testEquality (typeRep @era) (typeRep @StandardAurum) of
            Just Refl{} ->
                Just $ frequency
                    [ (1, Left <$> genMismatchEraInfo)
                    , (10, Right <$> arbitrary)
                    ]
            Nothing ->
                Nothing

genPoolDistrResult
    :: forall crypto. (crypto ~ StandardCrypto)
    => Proxy (QueryResult crypto (Ledger.PoolDistr crypto))
    -> Gen (QueryResult crypto (Ledger.PoolDistr crypto))
genPoolDistrResult _ = frequency
    [ (1, Left <$> genMismatchEraInfo)
    , (10, Right <$> reasonablySized arbitrary)
    ]

genUTxOResult
    :: forall crypto era. (crypto ~ StandardCrypto, Typeable era)
    => Proxy era
    -> Proxy (QueryResult crypto (Sh.UTxO era))
    -> Gen (QueryResult crypto (Sh.UTxO era))
genUTxOResult _ _ =
    maybe (error "genProposedPParamsResult: unsupported era") reasonablySized
        (genSophie <|> genAllegra <|> genJen <|> genAurum)
  where
    genSophie =
        case testEquality (typeRep @era) (typeRep @StandardSophie) of
            Just Refl{} ->
                Just $ frequency
                    [ (1, Left <$> genMismatchEraInfo)
                    , (10, Right <$> arbitrary)
                    ]
            Nothing ->
                Nothing
    genAllegra =
        case testEquality (typeRep @era) (typeRep @StandardAllegra) of
            Just Refl{} ->
                Just $ frequency
                    [ (1, Left <$> genMismatchEraInfo)
                    , (10, Right <$> arbitrary)
                    ]
            Nothing ->
                Nothing
    genJen =
        case testEquality (typeRep @era) (typeRep @StandardJen) of
            Just Refl{} ->
                Just $ frequency
                    [ (1, Left <$> genMismatchEraInfo)
                    , (10, Right <$> arbitrary)
                    ]
            Nothing ->
                Nothing
    genAurum =
        case testEquality (typeRep @era) (typeRep @StandardAurum) of
            Just Refl{} ->
                Just $ frequency
                    [ (1, Left <$> genMismatchEraInfo)
                    , (10, Right <$> arbitrary)
                    ]
            Nothing ->
                Nothing


genCompactGenesisResult
    :: forall crypto era. (crypto ~ StandardCrypto, Typeable era)
    => Proxy era
    -> Proxy (QueryResult crypto (CompactGenesis era))
    -> Gen (QueryResult crypto (CompactGenesis era))
genCompactGenesisResult _ _ =
    maybe (error "genCompactGenesisResult: unsupported era") reasonablySized
        (genSophie <|> genAllegra <|> genJen <|> genAurum)
  where
    genSophie =
        case testEquality (typeRep @era) (typeRep @StandardSophie) of
            Just Refl{} ->
                Just $ frequency
                    [ (1, Left <$> genMismatchEraInfo)
                    , (10, Right . compactGenesis <$> arbitrary)
                    ]
            Nothing ->
                Nothing
    genAllegra =
        case testEquality (typeRep @era) (typeRep @StandardAllegra) of
            Just Refl{} ->
                Just $ frequency
                    [ (1, Left <$> genMismatchEraInfo)
                    , (10, Right . compactGenesis <$> arbitrary)
                    ]
            Nothing ->
                Nothing
    genJen =
        case testEquality (typeRep @era) (typeRep @StandardJen) of
            Just Refl{} ->
                Just $ frequency
                    [ (1, Left <$> genMismatchEraInfo)
                    , (10, Right . compactGenesis <$> arbitrary)
                    ]
            Nothing ->
                Nothing
    genAurum =
        case testEquality (typeRep @era) (typeRep @StandardAurum) of
            Just Refl{} ->
                Just $ frequency
                    [ (1, Left <$> genMismatchEraInfo)
                    , (10, Right . compactGenesis <$> arbitrary)
                    ]
            Nothing ->
                Nothing

genRewardInfoPoolsResult
    :: forall crypto. (crypto ~ StandardCrypto)
    => Proxy (QueryResult crypto (RewardProvenance' crypto))
    -> Gen (QueryResult crypto (RewardProvenance' crypto))
genRewardInfoPoolsResult _ = frequency
    [ (1, Left <$> genMismatchEraInfo)
    , (10, Right <$> genRewardProvenance')
    ]

genRewardProvenanceResult
    :: forall crypto. (crypto ~ StandardCrypto)
    => Proxy (QueryResult crypto (RewardProvenance crypto))
    -> Gen (QueryResult crypto (RewardProvenance crypto))
genRewardProvenanceResult _ = frequency
    [ (1, Left <$> genMismatchEraInfo)
    , (10, Right <$> reasonablySized arbitrary)
    ]

genPoolIdsResult
    :: forall crypto. (crypto ~ StandardCrypto)
    => Proxy (QueryResult crypto (Set (Ledger.KeyHash 'StakePool crypto)))
    -> Gen (QueryResult crypto (Set (Ledger.KeyHash 'StakePool crypto)))
genPoolIdsResult _ = frequency
    [ (1, Left <$> genMismatchEraInfo)
    , (10, Right <$> reasonablySized arbitrary)
    ]

genPoolParametersResult
    :: forall crypto. (crypto ~ StandardCrypto)
    => Proxy (QueryResult crypto (Map (Ledger.KeyHash 'StakePool crypto) (PoolParams crypto)))
    -> Gen (QueryResult crypto (Map (Ledger.KeyHash 'StakePool crypto) (PoolParams crypto)))
genPoolParametersResult _ = frequency
    [ (1, Left <$> genMismatchEraInfo)
    , (10, Right <$> reasonablySized arbitrary)
    ]

genPoolsRankingResult
    :: forall crypto. (crypto ~ StandardCrypto)
    => Proxy (QueryResult crypto (RewardProvenance crypto))
    -> Gen (QueryResult crypto (RewardProvenance crypto))
genPoolsRankingResult =
    genRewardProvenanceResult

genMirror
    :: Gen (Maybe Json.Value)
genMirror = oneof
    [ pure Nothing
    , Just . Json.toJSON <$> arbitrary @Int
    ]

genUtxo
    :: Gen (UTxO (AurumEra StandardCrypto))
genUtxo =
    reasonablySized arbitrary

shrinkUtxo
    :: UTxO (AurumEra StandardCrypto)
    -> [UTxO (AurumEra StandardCrypto)]
shrinkUtxo (UTxO u) =
    UTxO . Map.fromList <$> shrinkList shrink (Map.toList u)

--
-- Helpers
--

reasonablySized :: Gen a -> Gen a
reasonablySized = scale (ceiling . sqrt @Double . fromIntegral)

generateWith :: Gen a -> Int -> a
generateWith (MkGen run) seed = run (mkQCGen seed) 30
