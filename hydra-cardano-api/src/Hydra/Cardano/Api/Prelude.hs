module Hydra.Cardano.Api.Prelude (
  module Cardano.Api,
  module Cardano.Api.Shelley,
  module Data.Aeson,
  HasCallStack,
  Proxy (..),
  Typeable,
  UTxO,
  UTxO' (UTxO),
  StandardCrypto,
  Era,
  LedgerEra,
  UsesStandardCrypto,
  Text,
  decodeUtf8,
  encodeUtf8,
  toStrict,
  fromStrict,
  ByteString,
  Map,
  Set,
  unsafeHashFromBytes,
  Arbitrary (..),
  Gen,
) where

import Cardano.Api hiding (
  UTxO,
  multiAssetSupportedInEra,
  scriptDataSupportedInEra,
  scriptLanguageSupportedInEra,
  toLedgerUTxO,
 )
import Cardano.Api.Shelley hiding (
  UTxO,
  multiAssetSupportedInEra,
  scriptDataSupportedInEra,
  scriptLanguageSupportedInEra,
  toLedgerUTxO,
 )
import Cardano.Api.UTxO (UTxO, UTxO' (..))
import qualified Cardano.Crypto.Hash.Class as CC
import qualified Cardano.Ledger.Babbage as Ledger
import Cardano.Ledger.Crypto (StandardCrypto)
import qualified Cardano.Ledger.Era as Ledger.Era
import Data.Aeson (FromJSON (..), ToJSON (..))
import Data.ByteString (ByteString)
import Data.ByteString.Lazy (fromStrict, toStrict)
import Data.Map (Map)
import Data.Proxy (Proxy (..))
import Data.Set (Set)
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Data.Typeable (Typeable)
import GHC.Stack (HasCallStack)
import Test.QuickCheck (Arbitrary (..), Gen)

type Era = BabbageEra

type LedgerEra = Ledger.BabbageEra StandardCrypto

type UsesStandardCrypto era =
  (Ledger.Era.Crypto (ShelleyLedgerEra era) ~ StandardCrypto)

-- | Interpret some raw 'ByteString' as a particular 'Hash'.
--
-- NOTE: This throws if byte string has a length different that the expected
-- target digest length.
unsafeHashFromBytes ::
  (HasCallStack, CC.HashAlgorithm hash) =>
  ByteString ->
  CC.Hash hash a
unsafeHashFromBytes bytes =
  case CC.hashFromBytes bytes of
    Nothing ->
      error $ "unsafeHashFromBytes: failed to convert hash: " <> show bytes
    Just h ->
      h
