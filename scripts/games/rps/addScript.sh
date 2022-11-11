PLUTUS_SCRIPT="$ADAPLAYS/output/games/rps/validator.plutus"
REFERENCE_ADDRESS=$MY_ADDRESS
TXIN=$1
TXIN_2=$2  # just in case you wanna give some extra

cardano-cli transaction build \
--babbage-era \
--testnet-magic 1 \
--tx-in $TXIN \
--tx-in $TXIN_2 \
--tx-out $REFERENCE_ADDRESS+36000000 \
--tx-out-reference-script-file $PLUTUS_SCRIPT \
--change-address $MY_ADDRESS \
--protocol-params-file protocol.json \
--out-file tx.raw
