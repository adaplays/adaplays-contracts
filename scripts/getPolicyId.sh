# Sample usage: `bash getPolicyId.sh "../output/thread-token/validator.plutus"` if inside "scripts" folder or `bash ../../scripts/getPolicyId.sh validator.plutus` if in that "output/thread-token" folder.

cardano-cli transaction policyid --script-file $1
