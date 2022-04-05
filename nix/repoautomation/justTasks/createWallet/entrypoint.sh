# shellcheck shell=bash

_network="__argNetwork__"
_outfile="${_network}DbSyncWallet-$(date -u +"%Y-%m-%d_%H-%M-%SZ").json"

mkdir -p __argWorkbench__ && cd __argWorkbench__ || exit

MNEMONICS="$(cardano-address recovery-phrase generate)"
MNEMONICS_API="\\\\\\\"${MNEMONICS//[[:space:]]/'\\\", \\\"'}\\\\\\\""

VAULT_KEYS=(
  "cardanoWalletId"
  "cardanoWalletInitData"
  "cardanoWalletInitName"
  "cardanoWalletInitPass"
  "cardanoWalletNodeUrl"
)

VAULT_VALUES=(
  "REPLACE_CARDANO_WALLET_ID_FROM_LOGS_HERE_ONCE_GENERATED_AFTER_FIRST_JOB_RUN"
  "$MNEMONICS_API"
  "${_network}DbSyncWallet"
  "$(pwgen -n 32 -s 1)"
  "http://localhost:8090"
)

JSON="{}"
for ((i = 0; i < ${#VAULT_KEYS[@]}; i++)); do
  TMP_JSON=$(jq ". += { \"${VAULT_KEYS[$i]}\": \"${VAULT_VALUES[$i]}\" }" <<<"$JSON")
  JSON="$TMP_JSON"
done

echo "$JSON" >"${_outfile}"
echo "Initialization vault parameters for dbSync wallet on Cardano network ${_network}, stored at:"
echo "> $(realpath "${_outfile}")"
echo
echo "Note that once the dbSync job for Cardano network ${_network} has run the first time,"
echo "the proper wallet ID needs to be obtained from the nomad job logs and updated"
echo "in the CARDANO_WALLET_ID vault parameter value for the appropriate namespace."
