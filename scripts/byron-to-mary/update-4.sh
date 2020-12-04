#!/usr/bin/env bash

set -e
# set -x

# This script will initiate the transition to protocol version 4 (Mary).

# You need to provide the current epoch as a positional argument (the Shelley
# update system requires this to be includded in the update proposal).


# In order for this to be successful, you need to already be in protocol version
# 3 (which happens one or two epoch boundaries after invoking update-3.sh).
# Also, you need to restart the nodes after running this script in order for the
# update to be endorsed by the nodes.

EPOCH=$1
VERSION=4

ROOT=example
COINS_IN_INPUT=450000000

pushd ${ROOT}

export CARDANO_NODE_SOCKET_PATH=node-bft1/node.sock

TXHASH=$(cardano-cli query utxo --allegra-era --testnet-magic 42 | grep ${COINS_IN_INPUT} | awk '{print $1;}')

# Create the update proposal to change the protocol version to 4

cardano-cli governance create-update-proposal \
            --out-file update-proposal-mary \
            --epoch ${EPOCH} \
            --genesis-verification-key-file shelley/genesis-keys/genesis1.vkey \
            --genesis-verification-key-file shelley/genesis-keys/genesis2.vkey \
            --protocol-major-version ${VERSION} \
            --protocol-minor-version 0

# Create a transaction body containing the update proposal.

cardano-cli transaction build-raw \
            --allegra-era \
            --fee 0 \
            --tx-in $TXHASH#0\
            --tx-out $(cat addresses/user1.addr)+${COINS_IN_INPUT} \
            --update-proposal-file update-proposal-mary \
            --out-file tx2.txbody

# Sign the transaction body with the two genesis delegate keys,
# and the the uxto spending key.

cardano-cli transaction sign \
            --signing-key-file addresses/user1.skey \
            --signing-key-file shelley/delegate-keys/delegate1.skey \
            --signing-key-file shelley/delegate-keys/delegate2.skey \
            --testnet-magic 42 \
            --tx-body-file  tx2.txbody \
            --out-file      tx2.tx


cardano-cli transaction submit --tx-file tx2.tx --testnet-magic 42

sed -i configuration.yaml \
    -e 's/LastKnownBlockVersion-Major: 3/LastKnownBlockVersion-Major: 4/' \


popd

echo "Restart the nodes now to endorse the update."
