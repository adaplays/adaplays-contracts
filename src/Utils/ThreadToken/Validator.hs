{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
--
module Utils.ThreadToken.Validator (Action (..), policy, curSymbol) where
--
import           Plutus.Script.Utils.V2.Scripts                        (scriptCurrencySymbol)
import           Plutus.Script.Utils.V2.Typed.Scripts.MonetaryPolicies (mkUntypedMintingPolicy)
import           Plutus.V1.Ledger.Value
import           Plutus.V2.Ledger.Api
import qualified PlutusTx
import           PlutusTx.Prelude                                      hiding
                                                                       (Semigroup (..),
                                                                        unless)

data Action = Mint | Burn

PlutusTx.unstableMakeIsData ''Action

{-# INLINEABLE mkPolicy #-}
mkPolicy :: TxOutRef -> TokenName -> Action -> ScriptContext -> Bool
mkPolicy oref tn action ctx = case action of

  Mint -> traceIfFalse "UTxO not present." hasUTxO && checkMintAmount

  Burn -> checkBurnAmount

  where

    info :: TxInfo
    info = scriptContextTxInfo ctx

    hasUTxO :: Bool
    hasUTxO = any (\i -> txInInfoOutRef i == oref) $ txInfoInputs info

    checkMintAmount :: Bool
    checkMintAmount = case flattenValue (txInfoMint info) of
      [(_, tn', amt)] -> traceIfFalse "Wrong token name." (tn' == tn) && traceIfFalse "Wrong mint amount." (amt == 1)
      []              -> traceError "No mint present."
      _ : _ : _       -> traceError "Must mint exactly one token."

    checkBurnAmount :: Bool
    checkBurnAmount = case flattenValue (txInfoMint info) of
      [(_, tn', amt)] -> traceIfFalse "Wrong token name." (tn' == tn) && traceIfFalse "Wrong mint amount." (amt == -1)
      []              -> traceError "No mint present."
      _ : _ : _       -> traceError "Must mint exactly one token."

policy :: TxOutRef -> TokenName -> MintingPolicy
policy o n = mkMintingPolicyScript ($$(PlutusTx.compile [|| \oref' tn' -> wrap $ mkPolicy oref' tn' ||]) `PlutusTx.applyCode` PlutusTx.liftCode o `PlutusTx.applyCode` PlutusTx.liftCode n)
  where
    wrap = mkUntypedMintingPolicy

curSymbol :: TxOutRef -> TokenName -> CurrencySymbol
curSymbol oref tn = scriptCurrencySymbol $ policy oref tn
