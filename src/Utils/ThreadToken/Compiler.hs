{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Utils.ThreadToken.Compiler (writeRedeemerMint, writeRedeemerBurn, writeThreadTokenPolicy, writeThreadTokenPolicyWithoutParam) where

import           Cardano.Api
import           Plutus.V1.Ledger.Tx         (TxOutRef (..))
import qualified Plutus.V2.Ledger.Api
import           PlutusTx.Prelude            hiding (Semigroup (..), unless)
import           Prelude                     (IO)
import           Utils.Common
import qualified Utils.ThreadToken.Validator as Validator

writeRedeemerMint :: IO ()
writeRedeemerMint = writeJSON "output/thread-token/redeemer-mint.json" Validator.Mint

writeRedeemerBurn :: IO ()
writeRedeemerBurn = writeJSON "output/thread-token/redeemer-burn.json" Validator.Burn

-- `TxOutRef`: https://cardano.stackexchange.com/a/9395/7049
writeThreadTokenPolicy :: IO (Either (FileError ()) ())
writeThreadTokenPolicy = writePolicy "output/thread-token/validator.plutus" $ Validator.policy (TxOutRef "9517d184c01e68c97b1605ccb009591e259c020841ee28640fba688077bf35fb" 39) (Plutus.V2.Ledger.Api.TokenName "RPS")

writeThreadTokenPolicyWithoutParam :: IO (Either (FileError ()) ())
writeThreadTokenPolicyWithoutParam = writePolicy "output/thread-token/validatorWithoutParam.plutus" Validator.policyWithoutParam
