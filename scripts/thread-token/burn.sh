POLICY_ID=$1
TOKEN_NAME=$2
TXIN=$3
TXIN_TOKEN=$4
SCRIPT_FILE=$ADAPLAYS/output/thread-token/validator.plutus
REDEEMER_FILE=$ADAPLAYS/output/thread-token/redeemer-burn.json
cardano-cli transaction build \
  --babbage-era \
  --testnet-magic $MAGIC \
  --tx-in $TXIN \
  --tx-in $TXIN_TOKEN \
  --change-address $MY_ADDRESS \
  --mint "-1 $POLICY_ID.$TOKEN_NAME" \
  --mint-script-file $SCRIPT_FILE \
  --mint-redeemer-file $REDEEMER_FILE \
  --tx-in-collateral $TXIN \
  --protocol-params-file protocol.json \
  --out-file tx.raw
