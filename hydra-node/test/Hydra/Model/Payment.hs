{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | A simplistic type of transactions useful for modelling purpose.
-- a `Payment` is a simple transaction type that moves some amount of ADAs between
-- to `CardanoSigningKey`.
module Hydra.Model.Payment where

import Hydra.Cardano.Api
import Hydra.Prelude hiding (Any, label)

import qualified Data.List as List
import Hydra.Chain.Direct.Fixture (testNetworkId)
import Hydra.Ledger (IsTx (..))
import Hydra.Ledger.Cardano (genKeyPair)
import Test.Consensus.Cardano.Generators ()
import Test.QuickCheck (choose)
import Test.QuickCheck.StateModel (HasVariables)
import Test.QuickCheck.StateModel.Variables (HasVariables (..))
import qualified Prelude

newtype CardanoSigningKey = CardanoSigningKey {signingKey :: SigningKey PaymentKey}

instance Show CardanoSigningKey where
  show CardanoSigningKey{signingKey} =
    show . mkVkAddress @Era testNetworkId . getVerificationKey $ signingKey

-- NOTE: We need this orphan instance in order to lookup keys in lists.
instance Eq CardanoSigningKey where
  CardanoSigningKey (PaymentSigningKey skd) == CardanoSigningKey (PaymentSigningKey skd') = skd == skd'

instance ToJSON CardanoSigningKey where
  toJSON = error "don't use"

instance FromJSON CardanoSigningKey where
  parseJSON = error "don't use"

instance Arbitrary Value where
  arbitrary = genAdaValue

instance Arbitrary CardanoSigningKey where
  arbitrary = CardanoSigningKey . snd <$> genKeyPair

-- | A single Ada-payment only transaction in our model.
data Payment = Payment
  { from :: CardanoSigningKey
  , to :: CardanoSigningKey
  , value :: Value
  }
  deriving (Eq, Generic, ToJSON, FromJSON)

instance Show Payment where
  -- NOTE: We display derived addresses instead of raw signing keys in order to help troubleshooting
  -- tests failures or errors.
  show Payment{from, to, value} =
    "Payment { from = "
      <> show from
      <> ", to = "
      <> show to
      <> ", value = "
      <> show value
      <> " }"

instance Arbitrary Payment where
  arbitrary = error "don't use"

instance ToCBOR Payment where
  toCBOR = error "don't use"

instance FromCBOR Payment where
  fromCBOR = error "don't use"

instance HasVariables Payment where
  getAllVariables _ = mempty

-- | Making `Payment` an instance of `IsTx` allows us to use it with `HeadLogic'`s messages.
instance IsTx Payment where
  type TxIdType Payment = Int
  type UTxOType Payment = [(CardanoSigningKey, Value)]
  type ValueType Payment = Value
  txId = error "undefined"
  balance = foldMap snd
  hashUTxO = encodeUtf8 . show @Text

applyTx :: UTxOType Payment -> Payment -> UTxOType Payment
applyTx utxo Payment{from, to, value} =
  (to, value) : List.delete (from, value) utxo

genAdaValue :: Gen Value
genAdaValue = lovelaceToValue . fromInteger <$> choose (1, 10000000000)
