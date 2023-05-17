{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Test.GeneratorSpec where

import Hydra.Prelude
import Test.Hydra.Prelude

import Cardano.Ledger.Shelley.API (mkShelleyGlobals)
import Data.Text (unpack)
import Hydra.Cardano.Api (LedgerEra, UTxO, prettyPrintJSON, protocolParamProtocolVersion, utxoFromTx)
import Hydra.Cluster.Fixture (Actor (Faucet))
import Hydra.Cluster.Util (keysFor)
import Hydra.Generator (
  ClientDataset (..),
  Dataset (..),
  defaultProtocolParameters,
  genDatasetConstantUTxO,
 )
import Hydra.Ledger (ChainSlot (ChainSlot), applyTransactions)
import Hydra.Ledger.Cardano (Tx, cardanoLedger)
import Hydra.Ledger.Cardano.Configuration (
  Globals,
  LedgerEnv,
  newLedgerEnv,
  protocolParametersFromJson,
  readJsonFileThrow,
  shelleyGenesisFromJson,
 )
import qualified Hydra.Ledger.Cardano.Evaluate as Fixture
import Test.Aeson.GenericSpecs (roundtripSpecs)
import Test.QuickCheck (
  Positive (Positive),
  Property,
  counterexample,
  forAll,
  idempotentIOProperty,
 )

spec :: Spec
spec = parallel $ do
  roundtripSpecs (Proxy @Dataset)
  prop "generates a Dataset that keeps UTXO constant" prop_keepsUTxOConstant

prop_keepsUTxOConstant :: Property
prop_keepsUTxOConstant =
  forAll arbitrary $ \(Positive n) -> do
    idempotentIOProperty $ do
      faucetSk <- snd <$> keysFor Faucet

      shelleyGenesis <- readJsonFileThrow shelleyGenesisFromJson "config/devnet/genesis-shelley.json"
      let (majV, _) = protocolParamProtocolVersion Fixture.pparams
      let globals = mkShelleyGlobals shelleyGenesis Fixture.epochInfo majV

      ledgerEnv <-
        newLedgerEnv
          <$> readJsonFileThrow protocolParametersFromJson "config/protocol-parameters.json"
      -- XXX: non-exhaustive pattern match
      pure $
        forAll (genDatasetConstantUTxO faucetSk defaultProtocolParameters 1 n) $
          \Dataset{fundingTransaction, clientDatasets = [ClientDataset{txSequence}]} ->
            let initialUTxO = utxoFromTx fundingTransaction
                finalUTxO = foldl' (apply globals ledgerEnv) initialUTxO txSequence
             in length finalUTxO == length initialUTxO
                  & counterexample ("transactions: " <> prettyJSONString txSequence)
                  & counterexample ("utxo: " <> prettyJSONString initialUTxO)
                  & counterexample ("funding tx: " <> prettyJSONString fundingTransaction)

apply :: Globals -> LedgerEnv LedgerEra -> UTxO -> Tx -> UTxO
apply globals ledgerEnv utxo tx =
  case applyTransactions (cardanoLedger globals ledgerEnv) (ChainSlot 0) utxo [tx] of
    Left err -> error $ "invalid generated data set" <> show err
    Right finalUTxO -> finalUTxO

prettyJSONString :: ToJSON a => a -> String
prettyJSONString = unpack . decodeUtf8 . prettyPrintJSON
