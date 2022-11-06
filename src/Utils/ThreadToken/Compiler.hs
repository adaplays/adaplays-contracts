{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}

module Utils.ThreadToken.Compiler (writeRedeemerMint, writeRedeemerBurn, writeThreadTokenPolicy) where

import           Cardano.Api
import           Cardano.Api.Shelley         (PlutusScript (..))
import           Codec.Serialise             (serialise)
import           Data.Aeson                  (encode)
import qualified Data.ByteString.Lazy        as LBS
import qualified Data.ByteString.Short       as SBS
import           Plutus.V1.Ledger.Tx         (TxOutRef (..))
import qualified Plutus.V2.Ledger.Api
import           PlutusTx                    (Data (..))
import qualified PlutusTx
import           PlutusTx.Prelude            hiding (Semigroup (..), unless)
import           Prelude                     (FilePath, IO)
import qualified Utils.ThreadToken.Validator as Validator

dataToScriptData :: Data -> ScriptData
dataToScriptData (Constr n xs) = ScriptDataConstructor n $ dataToScriptData <$> xs
dataToScriptData (Map xs)      = ScriptDataMap [(dataToScriptData x, dataToScriptData y) | (x, y) <- xs]
dataToScriptData (List xs)     = ScriptDataList $ dataToScriptData <$> xs
dataToScriptData (I n)         = ScriptDataNumber n
dataToScriptData (B bs)        = ScriptDataBytes bs

writeJSON :: PlutusTx.ToData a => FilePath -> a -> IO ()
writeJSON file = LBS.writeFile file . encode . scriptDataToJson ScriptDataJsonDetailedSchema . dataToScriptData . PlutusTx.toData

writeRedeemerMint :: IO ()
writeRedeemerMint = writeJSON "output/thread-token/redeemer-mint.json" Validator.Mint

writeRedeemerBurn :: IO ()
writeRedeemerBurn = writeJSON "output/thread-token/redeemer-burn.json" Validator.Burn

writePolicy :: FilePath -> Plutus.V2.Ledger.Api.MintingPolicy -> IO (Either (FileError ()) ())
writePolicy file = writeFileTextEnvelope @(PlutusScript PlutusScriptV2) file Nothing . PlutusScriptSerialised . SBS.toShort . LBS.toStrict . serialise . Plutus.V2.Ledger.Api.unMintingPolicyScript

-- `TxOutRef`: https://cardano.stackexchange.com/a/9395/7049
writeThreadTokenPolicy :: IO (Either (FileError ()) ())
writeThreadTokenPolicy = writePolicy "output/thread-token/validator.plutus" $ Validator.policy (TxOutRef "f2d0d3139bcc34e274261e28b8d0afbcfdc6e36c7f4951193451080de82a57cf" 0) (Plutus.V2.Ledger.Api.TokenName "RPS")
