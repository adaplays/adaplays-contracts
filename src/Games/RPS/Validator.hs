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
-- To explore
-- Do we need to consider only payment key when checking "is signed by"?
--
module Games.RPS.Validator (Move (..), MatchResult (..), GameParams (..), GameDatum (..), GameRedeemer (..), validator) where
--
import           Ledger.Ada                           (fromValue, getLovelace)
import           Plutus.Script.Utils.V2.Typed.Scripts (mkUntypedValidator)
import           Plutus.V1.Ledger.Address
import           Plutus.V1.Ledger.Interval
import           Plutus.V1.Ledger.Time
import           Plutus.V1.Ledger.Value
import           Plutus.V2.Ledger.Api
import           Plutus.V2.Ledger.Contexts
import qualified PlutusTx
import           PlutusTx.Prelude                     hiding (Semigroup (..),
                                                       unless)
-- import qualified Utils.ThreadToken.Validator          as M

data Move = Rock | Paper | Scissors

instance Eq Move where
  {-# INLINABLE (==) #-}
  Rock     == Rock     = True
  Paper    == Paper    = True
  Scissors == Scissors = True
  _        == _        = False

PlutusTx.makeIsDataIndexed ''Move [('Rock, 0), ('Paper, 1), ('Scissors, 2)]

data MatchResult = WinA | WinB | Draw

instance Eq MatchResult where
  {-# INLINABLE (==) #-}
  WinA     == WinA = True
  WinB    == WinB  = True
  Draw == Draw     = True
  _        == _    = False

PlutusTx.makeIsDataIndexed ''MatchResult [('WinA, 0), ('WinB, 1), ('Draw, 2)]

-- Though I want gStake to be greater than or equal to 2 ADA in frontend, but I haven't enforced that here as I don't want UTxO's to remain stagnant.
-- Thread token is something this contract doesn't (and perhaps can't) check whether its an nft or not.
data GameParams = GameParams
  { gPlayerA        :: !Address
  , gPlayerB        :: !Address
  , gStake          :: !Integer
  , gStartTime      :: !POSIXTime
  , gMoveDuration   :: !DiffMilliSeconds
  , gToken          :: !AssetClass
  , gTokenORef      :: !TxOutRef
  , gPbkdf2Iv       :: !BuiltinByteString
  , gPbkdf2Iter     :: !Integer
  , gEncryptedNonce :: !BuiltinByteString
  , gEncryptIv      :: !BuiltinByteString
  }

instance Eq GameParams where
  paramA == paramB = gPlayerA paramA == gPlayerA paramB
                  && gPlayerB paramA == gPlayerB paramB
                  && gStake paramA == gStake paramB
                  && gStartTime paramA == gStartTime paramB
                  && gMoveDuration paramA == gMoveDuration paramB
                  && gToken paramA == gToken paramB
                  && gTokenORef paramA == gTokenORef paramB
                  && gPbkdf2Iv paramA == gPbkdf2Iv paramB
                  && gPbkdf2Iter paramA == gPbkdf2Iter paramB
                  && gEncryptedNonce paramA == gEncryptedNonce paramB
                  && gEncryptIv paramA == gEncryptIv paramB

PlutusTx.makeIsDataIndexed ''GameParams [('GameParams, 0)]

data GameDatum = GameDatum
  { gParams      :: !GameParams
  , gFirstMove   :: !BuiltinByteString
  , gSecondMove  :: !(Maybe Move)
  , gMatchResult :: !(Maybe MatchResult)
  }

PlutusTx.makeIsDataIndexed ''GameDatum [('GameDatum, 0)]

data GameRedeemer = BMove !Move | Reveal !BuiltinByteString !Move | BTimeoutTakeA | ATimeoutTakeB | DrawB | AIsIdiot

PlutusTx.makeIsDataIndexed ''GameRedeemer [('BMove, 0), ('Reveal, 1), ('BTimeoutTakeA, 2), ('ATimeoutTakeB, 3), ('DrawB, 4), ('AIsIdiot, 5)]

{-# INLINEABLE mkValidator #-}
mkValidator :: GameDatum -> GameRedeemer -> ScriptContext -> Bool
mkValidator dat red ctx =

  traceIfFalse "Thread token missing from input."
    (assetClassValueOf (txOutValue ownInput) (gToken gameParams) == 1) &&
  -- I do the following check off-chain. Since currency symbol is obtained from hash, it is next to impossible to have another script with restricted entropy (as I am giving tx reference and token name) to map to same image.
  -- traceIfFalse "Input token is not an NFT."
  --   (M.curSymbol (gTokenORef gameParams) (snd (unAssetClass (gToken gameParams))) == fst (unAssetClass (gToken gameParams))) &&

  case (gFirstMove dat, gSecondMove dat, gMatchResult dat, red) of

    -- Case when second player doesn't make a move.
    (_, Nothing, Nothing, BTimeoutTakeA) ->
      traceIfFalse "Not signed by first player."
        (txSignedBy info $ getPlayerPKH (gPlayerA gameParams)) &&
      traceIfFalse "Cannot claim before time duration given for second player's move."
        (from ((1 :: POSIXTime) + secondPlayerMoveDeadline) `contains` txInfoValidRange info) &&
      traceIfFalse "NFT must be burnt." nftBurnt

    -- Case when second player makes a move
    (bs, Nothing, Nothing, BMove move) ->
      traceIfFalse "Not signed by second player."
        (txSignedBy info $ getPlayerPKH $ gPlayerB gameParams) &&
      -- Though we assume that start state is valid, still adding this.
      traceIfFalse "First player's stake is missing."
        (lovelaces (txOutValue ownInput) == gStake gameParams) &&
      traceIfFalse "Second player's stake is missing."
        (lovelaces (txOutValue ownOutput) == (2 * gStake gameParams)) &&
      traceIfFalse "Wrong output datum."
        (gFirstMove ownOutputDatum == bs && gSecondMove ownOutputDatum == Just move && isNothing (gMatchResult ownOutputDatum)) &&
      traceIfFalse "Missed deadline."
        (interval (gStartTime gameParams) secondPlayerMoveDeadline `contains` txInfoValidRange info) &&
      traceIfFalse "Token missing from output."
        (assetClassValueOf (txOutValue ownOutput) (gToken gameParams) == 1)

    -- We have three possibilities, first player won, their was draw and lastly, second player won.
    -- We can only determine first two possibilities only when we receive nonce.
    -- So when first player reveals his nonce, we'll allow transfer of full amount or half depending upon whether he won or their was draw.
    (bs, Just moveB, Nothing, Reveal nonce moveA) ->
      traceIfFalse "Not signed by first player."
        (txSignedBy info $ getPlayerPKH $ gPlayerA gameParams) &&
      traceIfFalse "Commit mismatch."
        (checkNonce bs nonce moveA) &&
      traceIfFalse "Missed deadline."
        (interval (gStartTime gameParams) firstPlayerMoveDeadline `contains` txInfoValidRange info) &&
      case matchResult moveA moveB of
        WinA -> traceIfFalse "NFT must be burnt." nftBurnt
        Draw ->
          traceIfFalse "Wrong output datum."
            (gFirstMove ownOutputDatum == moveToByteString moveA && gSecondMove ownOutputDatum == Just moveB && outputMatchResult == Draw) &&
          traceIfFalse "Token missing from output."
            (assetClassValueOf (txOutValue ownOutput) (gToken gameParams) == 1) &&
          traceIfFalse "Second player's stake missing in draw output."
            (lovelaces (txOutValue ownOutput) == gStake gameParams)
        -- AIsIdiot
        WinB ->
          traceIfFalse "Wrong output datum."
            (gFirstMove ownOutputDatum == moveToByteString moveA && gSecondMove ownOutputDatum == Just moveB && outputMatchResult == WinB)                             &&
          traceIfFalse "Token missing from output."
            (assetClassValueOf (txOutValue ownOutput) (gToken gameParams) == 1) &&
          traceIfFalse "You lost, cannot take stake from game."
            (lovelaces (txOutValue ownOutput) == (2 * gStake gameParams))

    (_, Just _, Nothing, ATimeoutTakeB) ->
      traceIfFalse "Not signed by second player."
        (txSignedBy info $ getPlayerPKH $ gPlayerB gameParams) &&
      traceIfFalse "Cannot claim before time duration given for first player's move."
        (from ((1 :: POSIXTime) + firstPlayerMoveDeadline) `contains` txInfoValidRange info) &&
      traceIfFalse "NFT must be burnt." nftBurnt

    (_, Just _, Just WinB, AIsIdiot) ->
      traceIfFalse "Not signed by second player."
        (txSignedBy info $ getPlayerPKH $ gPlayerB gameParams) &&
      traceIfFalse "NFT must be burnt." nftBurnt

    (_, Just _, Just Draw, DrawB) ->
      traceIfFalse "Not signed by second player."
        (txSignedBy info $ getPlayerPKH $ gPlayerB gameParams) &&
      traceIfFalse "NFT must be burnt." nftBurnt

    _anyOtherMatch -> False

  where

    info :: TxInfo
    info = scriptContextTxInfo ctx

    gameParams :: GameParams
    gameParams = gParams dat

    getPlayerPKH :: Address -> PubKeyHash
    getPlayerPKH addr = case toPubKeyHash addr of
      Nothing  -> traceError "No PubKey credentials exist for this address."
      Just pkh -> pkh


    ownInput :: TxOut
    ownInput = case findOwnInput ctx of
      Nothing -> traceError "Game input missing."
      Just i  -> txInInfoResolved i

    ownOutput :: TxOut
    ownOutput =
      case filter (\o -> gParams (deserialiseDatum $ outputDatum o) == gParams dat) $ getContinuingOutputs ctx of
        []        -> traceError "No output at this script address having same parameters."
        [o]       -> o
        _ : _ : _ -> traceError "Expected exactly one game output."

    outputDatum :: TxOut -> Datum
    outputDatum o =
      case txOutDatum o of
        OutputDatum d     -> d
        NoOutputDatum     -> traceError "Game output doesn't have datum"
        OutputDatumHash _ -> traceError "Game output doesn't have datum inlined"

    deserialiseDatum :: Datum -> GameDatum
    deserialiseDatum d =
      case PlutusTx.fromBuiltinData $ getDatum d of
        Nothing -> traceError "Datum couldn't be deserialised"
        Just gd -> gd

    ownOutputDatum :: GameDatum
    ownOutputDatum = deserialiseDatum $ outputDatum ownOutput

    lovelaces :: Value -> Integer
    lovelaces = getLovelace . fromValue

    secondPlayerMoveDeadline :: POSIXTime
    secondPlayerMoveDeadline = gStartTime gameParams + fromMilliSeconds (gMoveDuration gameParams)

    firstPlayerMoveDeadline :: POSIXTime
    firstPlayerMoveDeadline = gStartTime gameParams + fromMilliSeconds (gMoveDuration gameParams) + fromMilliSeconds (gMoveDuration gameParams)

    nftBurnt :: Bool
    nftBurnt = assetClassValueOf (txInfoMint info) (gToken gameParams) == (-1)


    moveToByteString :: Move -> BuiltinByteString
    moveToByteString move = case move of
      Rock     -> "Rock"
      Paper    -> "Paper"
      Scissors -> "Scissors"

    checkNonce :: BuiltinByteString -> BuiltinByteString -> Move -> Bool
    checkNonce bs nonce move = sha2_256 (nonce `appendByteString` moveToByteString move) == bs

    outputMatchResult :: MatchResult
    outputMatchResult = case gMatchResult ownOutputDatum of
      Nothing -> traceError "Match result expected but output datum has nothing."
      Just mr -> mr

    defeats :: Move -> Move -> Bool
    defeats Rock Scissors  = True
    defeats Paper Rock     = True
    defeats Scissors Paper = True
    defeats _ _            = False

    matchResult :: Move -> Move -> MatchResult
    matchResult moveA moveB
      | moveA == moveB      = Draw
      | defeats moveA moveB = WinA
      | otherwise           = WinB

validator :: Validator
validator = mkValidatorScript $$(PlutusTx.compile [|| wrap ||])
  where wrap = mkUntypedValidator mkValidator
