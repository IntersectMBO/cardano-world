#!/usr/bin/env bash

set -e

# build first:
#> cabal v2-build --reorder-goals

# create tmux session:
#> tmux new-session -s 'Demo' -t demo

# EXTRA="--live-view"
EXTRA="
  --trace-block-fetch-decisions
  --trace-block-fetch-client
  --trace-block-fetch-server
  --trace-chain-db
  --trace-tx-inbound
  --trace-tx-outbound
  --trace-local-tx-submission-server
  --trace-mempool
  --trace-forge
  --trace-chain-sync-protocol
  --trace-block-fetch-protocol
  --trace-tx-submission-protocol
  --trace-local-chain-sync-protocol
  --trace-local-tx-submission-protocol
"

. $(dirname $0)/lib-node.sh

CMD="$(executable_runner cardano-node)"

# for logs:
mkdir -p logs/

PWD=$(pwd)

#tmux split-window -v
#tmux select-pane -t 0
tmux split-window -h
tmux split-window -v
tmux select-pane -t 0
tmux split-window -v

tmux select-pane -t 1
tmux send-keys "cd '${PWD}'; ${CMD} $(nodeargs 0 "${ALGO} $(echo -n ${EXTRA})") " C-m
tmux select-pane -t 2
tmux send-keys "cd '${PWD}'; ${CMD} $(nodeargs 1 "${ALGO} $(echo -n ${EXTRA})") " C-m
tmux select-pane -t 3
tmux send-keys "cd '${PWD}'; ${CMD} $(nodeargs 2 "${ALGO} $(echo -n ${EXTRA})") " C-m

