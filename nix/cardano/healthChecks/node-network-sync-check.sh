#!/bin/bash

[ -z "${socketPath:-}" ] && echo "socketPath env var must be set -- aborting" && exit 1
[ -z "${envFlag:-}" ] && echo "envFlag env var must be set -- aborting" && exit 1

# Cardano-node in nomad appears to throw `resource vanished (Broken pipe) errors` intermittently.
# These don't appear to affect the outcome of the call, so forcing an otherwise synced node
# to ignore these errors when the json status is still correct prevents service flapping.
# shellcheck disable=SC2034
STATUS="$(env CARDANO_NODE_SOCKET_PATH="${socketPath}" cardano-cli query tip "${envFlag}" 2>/dev/null || :)"
jq <<<"$STATUS" || :
jq -e '.syncProgress == "100.00"' <<<"$STATUS" || exit 1
