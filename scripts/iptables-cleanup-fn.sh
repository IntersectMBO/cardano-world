#!/usr/bin/env bash

# Source this file to use the iptables analysis/cleanup functions.


# Iptables reported allocs being less than Nomad reported allocs is generally not a problem;
# it just indicates iptables isn't needed for those jobs.
# A problem is indicated when iptables allocs are higher than Nomad reported allocs for a client.
eval_client_class_iptables() {
  CLASS="$1"
  CLIENTS=$(bitte info --json \
    | jq ".nodes[] \
    | select(.name | startswith(\"client\")) \
    | select(.nomad_client.NodeClass == \"$CLASS\")" \
  )
  for CLIENT in $(jq -r .priv_ip <<< "$CLIENTS"); do
    echo -n "Evaluating: $CLIENT"
    ALLOCS=$(jq -r "select(.priv_ip == \"$CLIENT\") \
      | .nomad_client.allocs \
      | map(select(.ClientStatus == \"running\")) \
      | length" <<< "$CLIENTS" \
    )
    IPT=$(bitte ssh "$CLIENT" -- 'iptables-save | grep -Ec "\-A POSTROUTING.*nomad"');
    if [ "$ALLOCS" != "$IPT" ]; then
      echo " ... Nomad reported allocs ($ALLOCS) don't match iptables rules allocs ($IPT)"
    else
      echo " ... ok"
    fi
  done
}

clean_remote_iptables() {
  REMOTE="$1"
  TABLE=$(bitte ssh "$REMOTE" \
    -- iptables-save \
    | grep -E '\-A POSTROUTING.*nomad' \
    | sed -n -E 's|-A POSTROUTING -s ([^/]+).* id: \\"([^\\]+).* -j (.*$)|\2 \1 \3|p' \
  )
  readarray -t ROWS <<< "$TABLE"

  # Iterate over iptables declared nomad allocs
  for ROW in "${ROWS[@]}"; do
    # shellcheck disable=SC2206
    ARR=($ROW)
    ID=${ARR[0]}
    IP=${ARR[1]}
    CNI=${ARR[2]}
    CNI_DN="CNI-DN-${CNI:4:21}"
    MATCH="$ID|$IP|$CNI|$CNI_DN"

    # See if an iptables alloc actually exists; if not purge it
    if ! curl -sf -H "X-Nomad-Token: $NOMAD_TOKEN" "$NOMAD_ADDR/v1/allocation/$ID" &> /dev/null; then
      read -p "Allocation $ID does not exist -- do you wish to clean up the iptables? " -n 1 -r
      if [[ $REPLY =~ ^[Yy]$ ]]; then
        bitte ssh "$REMOTE" \
          -- bash -c "iptables-save \
          | grep -E \"$MATCH\" \
          | grep -v -E '^:CNI' \
          | sed -E 's|^-A|-D|g' \
          | xargs -d '\n' -I{} bash -c \"iptables -t filter {} &> /dev/null || iptables -t nat {} &> /dev/null\"
          iptables -t nat -X \"$CNI\"
          iptables -t nat -X \"$CNI_DN\""
      fi
    fi
  done
}
