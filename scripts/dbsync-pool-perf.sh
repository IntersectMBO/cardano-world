#!/usr/bin/env bash
set -euo pipefail

# Required env vars provided automatically by direnv shell
[ -z "${NOMAD_ADDR:-}" ] && { echo "The env var NOMAD_ADDR needs to be set and is normally provided by the direnv environment"; exit 1; }
[ -z "${NOMAD_TOKEN:-}" ] && { echo "The env var NOMAD_TOKEN needs to be set and is normally provided by the direnv environment"; exit 1; }

NS="infra"
JOB="database"
TASK="patroni"
SCRIPT_DIR=$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" &> /dev/null && pwd)
SQL_FILE="$SCRIPT_DIR/dbsync-pool-perf.sql"

PROMPT_CHECK() {
  MSG="$1"
  echo
  read -p "$MSG" -n 1 -r
  echo

  [[ "${REPLY:-n}" =~ ^[Yy]$ ]] && return 0
  return 1
}

INPUT() {
  MSG="$1"
  read -p "$MSG" -r
  echo "$REPLY"
}

DB_NS=$(INPUT "Which nomad namespace should be used for the dbsync pool analysis?: ")
echo

if ! [[ " preview preprod " =~ .*\ $DB_NS\ .* ]]; then
  echo "The nomad namespace for dbsync pool analysis must be one of:"
  echo "  preview"
  echo "  preprod"
  exit 1
fi

# Get random patroni allocation ID in namespace NS (patroni leader is not required)
ALLOC=$(
  curl \
    --fail \
    --silent \
    --location \
    --Header "X-Nomad-Token: $NOMAD_TOKEN" \
    "$NOMAD_ADDR/v1/job/$JOB/allocations?namespace=$NS" \
  | jq -e -r 'map(.ID)[0]'
)
echo "Random patroni allocation to be queried: $ALLOC"

# Command setup
EXEC="nomad exec -namespace $NS -task $TASK $ALLOC /bin/bash -c"
PATRONI_LIST="/nix/store/*-patroni-*/bin/patronictl list"
SQL=$(cat "$SQL_FILE")
PLACE_SQL_FILE=$(echo -ne "/nix/store/*-coreutils-*/bin/cat << 'EOF' > /local/dbsync-pool-perf.sql\n$SQL\nEOF")
RUN_QUERY=$(echo -n "/nix/store/*-postgresql-*/bin/psql -h /alloc -U dba -d ${DB_NS}_dbsync -x -P pager=off -f /local/dbsync-pool-perf.sql")

echo "Patroni cluster status is:"
$EXEC "$PATRONI_LIST"
PROMPT_CHECK "Do you wish to proceed with the current status? [y/n] " || exit 0
echo

echo "Placing sql file in allocation..."
$EXEC "$PLACE_SQL_FILE"

echo "Running query..."
QUERY=$($EXEC "$RUN_QUERY")
echo

echo "Query output:"
echo "$QUERY" | tail -n +2
echo

echo "JSON for database namespace $DB_NS is:"
JSON=$(grep -oP '^faucet_pool_summary_json[[:space:]]+\| \K{.*$' <<< "$QUERY" | jq .)
echo "$JSON"
echo

echo "Faucet pools to de-delegate are:"
jq '.faucet_to_dedelegate' <<< "$JSON"
echo

echo "The string of indexes of faucet pools to de-delegate from the JSON above are:"
jq '.faucet_to_dedelegate | to_entries | map(.key) | join(" ")' <<< "$JSON"
echo

MAX_SHIFT=$(grep -oP '^faucet_pool_to_dedelegate_shift_pct[[:space:]]+\| \K.*$' <<< "$QUERY")
echo "The maximum difference de-delegation of all these pools will make in chain density is: $MAX_SHIFT%"

exit
