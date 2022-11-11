{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Games.RPS.Compiler (writeDatum, writeRedeemer, writeRPSValidator) where

import           Cardano.Api
import           Games.RPS.Validator
import           Ledger.Address            (PaymentPubKeyHash (..))
import           Plutus.V1.Ledger.Value
import           Plutus.V2.Ledger.Contexts
import           PlutusTx.Prelude          hiding (Semigroup (..), unless)
import           Prelude                   (IO)
import           Utils.Common

gameParams :: GameParams
gameParams =
  GameParams {
    gPlayerA = PaymentPubKeyHash "e822059c8b3b42440ad9f33631af3c254df77088a618cee706ba3355"
  , gPlayerB = PaymentPubKeyHash "99a6d6253a56c17ee206cf275c7381d946a0e24260a753a14b08455f"
  , gStake = 3000000
  , gStartTime = 1667795400000
  , gMoveDuration = 180000  -- 3 minutes
  , gToken = AssetClass ("f4c5b6cd2784e7e4ed3a979382e8d8eb80c534d6e2a2580a2327fc45", "RPS")
  , gTokenORef = TxOutRef "25542391abc3c25b601eda01a3bc459154c33b63fecb91c0363a3addba45ffba" 0
  , gPbkdf2Iv = "someHexString"
  , gPbkdf2Iter = 8
  , gEncryptedNonce = "someHexString"
  , gEncryptIv = "someHexString"
  }

gameDatum :: GameDatum
gameDatum =
  GameDatum {
    gParams = gameParams,
    gFirstMove = "3c5b97ca4d548e5cbb1dbf5815c79fb18f1307e49265092e0ba0e37f97c391f0",  -- sha-256 of "113Paper"
    gSecondMove = Just Paper,
    gMatchResult = Just WinB
  }
--

writeDatum :: IO ()
writeDatum = writeJSON "output/games/rps/datum.json" gameDatum

gameRedeemer :: GameRedeemer
gameRedeemer = BTimeoutTakeA

writeRedeemer :: IO ()
writeRedeemer = writeJSON "output/games/rps/redeemer.json" gameRedeemer

writeRPSValidator :: IO (Either (FileError ()) ())
writeRPSValidator = writeValidator "output/games/rps/validator.plutus" validator
