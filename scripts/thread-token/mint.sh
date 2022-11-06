POLICY_ID=$1
TOKEN_NAME=$2
TXIN=$3
SCRIPT_FILE=$ADAPLAYS/output/thread-token/validator.plutus
REDEEMER_FILE=$ADAPLAYS/output/thread-token/redeemer-mint.json
cardano-cli transaction build \
  --babbage-era \
  --testnet-magic $MAGIC \
  --tx-in $TXIN \
  --tx-out $MY_ADDRESS+1500000+"1 $POLICY_ID.$TOKEN_NAME" \
  --change-address $MY_ADDRESS \
  --mint "1 $POLICY_ID.$TOKEN_NAME" \
  --mint-script-file $SCRIPT_FILE \
  --mint-redeemer-file $REDEEMER_FILE \
  --tx-in-collateral $TXIN \
  --protocol-params-file protocol.json \
  --out-file tx.raw
