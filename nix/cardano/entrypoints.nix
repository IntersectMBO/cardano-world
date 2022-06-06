{
  inputs,
  cell,
}: let
  inherit (inputs) nixpkgs;
  inherit (inputs.bitte-cells._writers.library) writeShellApplication;
  inherit (inputs.bitte-cells._utils.packages) srvaddr;
  inherit (cell) packages environments library;

  prelude-runtime = [nixpkgs.coreutils nixpkgs.curl nixpkgs.jq nixpkgs.xxd srvaddr];

  prelude = ''
    [ -z "''${DATA_DIR:-}" ] && echo "DATA_DIR env var must be set -- aborting" && exit 1

    mkdir -p "$DATA_DIR"
    mkdir -p "$DATA_DIR/config"
    chmod -R +w "$DATA_DIR/config"
    mkdir -p "$DATA_DIR/config/custom"
    chmod -R +w "$DATA_DIR/config/custom"

    # the menu of environments that we ship as built-in envs
    ${library.copyEnvsTemplate environments}

    # the legacy implementation to access kv config
    ${legacy-kv-config-instrumentation}

    # CASE: built-in environment
    if [ -n "''${ENVIRONMENT:-}" ]; then
      echo "Using the preset environment $ENVIRONMENT ..." > /dev/stderr

      NODE_CONFIG="$DATA_DIR/config/$ENVIRONMENT/config.json"
      NODE_TOPOLOGY="''${NODE_TOPOLOGY:-$DATA_DIR/config/$ENVIRONMENT/topology.json}"

    # CASE: premissioned long running environment
    elif [ -n "''${CONSUL_KV_PATH:-}" ] || [ -n "''${VAULT_KV_PATH:-}" ]; then
      echo "Using a long running environment as defined by kv (consul & vault) ..." > /dev/stderr

      load_kv_config
      [ "''${producer:-}" == "1" ] && load_kv_secrets

    # CASE: permissioned short running environment
    else
      echo "Using custom config: $NODE_CONFIG ..." > /dev/stderr

      [ -z "''${NODE_CONFIG:-}" ] && echo "NODE_CONFIG env var must be set -- aborting" && exit 1
    fi
  '';

  pull-snapshot-deps = [
    nixpkgs.curl
    nixpkgs.coreutils
    nixpkgs.gnutar
    nixpkgs.gzip
  ];
  pull-snapshot = ''
    function pull_snapshot {
      [ -z "''${SNAPSHOT_BASE_URL:-}" ] && echo "SNAPSHOT_BASE_URL env var must be set -- aborting" && exit 1
      [ -z "''${SNAPSHOT_FILE_NAME:-}" ] && echo "SNAPSHOT_FILE_NAME env var must be set -- aborting" && exit 1
      [ -z "''${DATA_DIR:-}" ] && echo "DATA_DIR env var must be set -- aborting" && exit 1

      SNAPSHOT_DIR="$DATA_DIR/initial-snapshot"
      mkdir -p "$SNAPSHOT_DIR"

      if curl -L "$SNAPSHOT_BASE_URL/$SNAPSHOT_FILE_NAME" --output "$SNAPSHOT_DIR/$SNAPSHOT_FILE_NAME"; then
        if curl -L "$SNAPSHOT_BASE_URL/$SNAPSHOT_FILE_NAME.sha256" --output "$SNAPSHOT_DIR/$SNAPSHOT_FILE_NAME.sha256"; then
          pushd "$SNAPSHOT_DIR"
          if sha256sum -c "$SNAPSHOT_FILE_NAME.sha256"; then
            echo "Pulled snapshot from $SNAPSHOT_BASE_URL/$SNAPSHOT_FILE_NAME."
          else
            echo "Could retrieve snapshot, but could not validate its checksum -- aborting" && exit 1
          fi
          popd
        else
          echo "Could retrieve snapshot, but not its sha256 file -- aborting" && exit 1
        fi
      else
        echo "No snapshot pulled -- aborting" && exit 1
      fi
    }
    function extract_snapshot_tgz_to {
      local targetDir="$1"
      if tar -C "$targetDir" -zxf "$SNAPSHOT_DIR/$SNAPSHOT_FILE_NAME"; then
        echo "Extracting snapshot to $targetDir complete."
      else
        echo "Extracting snapshot to $targetDir failed -- aborting" && exit 1
      fi
    }
  '';

  legacy-kv-config-instrumentation-db-sync = ''
    function load_kv_secrets_db_sync {

      [ -z "''${VAULT_KV_PATH:-}" ] && echo "VAULT_KV_PATH env var must be set -- aborting" && exit 1
      [ -z "''${VAULT_ADDR:-}" ] && echo "VAULT_ADDR env var must be set -- aborting" && exit 1
      [ -z "''${VAULT_TOKEN:-}" ] && echo "VAULT_TOKEN env var must be set -- aborting" && exit 1

      [ -z "''${WORKLOAD_CACERT:-}" ] && echo "WORKLOAD_CACERT env var must be set -- aborting" && exit 1
      [ -z "''${WORKLOAD_CLIENT_CERT:-}" ] && echo "WORKLOAD_CLIENT_CERT env var must be set -- aborting" && exit 1
      [ -z "''${WORKLOAD_CLIENT_KEY:-}" ] && echo "WORKLOAD_CLIENT_KEY env var must be set -- aborting" && exit 1

      export  VAULT_CACERT="$WORKLOAD_CACERT"
      export  VAULT_CLIENT_CERT="$WORKLOAD_CLIENT_CERT"
      export  VAULT_CLIENT_KEY="$WORKLOAD_CLIENT_KEY"

      local cmd=(
        "curl"
        "$VAULT_ADDR/v1/$VAULT_KV_PATH"
        "--header" "X-Vault-Token: $VAULT_TOKEN"
        "--header" "Content-Type: application/json"
      )

      local json
      json=$("''${cmd[@]}" | jq '.data.data')

      PGUSER=$(echo "$json"|jq -e -r '."pgUser"')
      PGPASS=$(echo "$json"|jq -e -r '."pgPass"')
      echo -n "$PSQL_ADDR0:$DB_NAME:$PGUSER:$PGPASS" > "$PGPASSFILE"
      test -z "''${PGPASSFILE:-}"            || chmod 0400 "$PGPASSFILE"
      set +x
    }
  '';

  legacy-kv-config-instrumentation = ''
    function ensure_file_location_contract {
      local key="$1"
      local value="$2"
      jq -e --arg KEY "$key" --arg VALUE "$value" '.[$KEY] == $VALUE' "$NODE_CONFIG" > /dev/null || \
        (echo "$value is not located where it needs to be ($(jq -r --arg KEY "$key" '.[$KEY]' "$NODE_CONFIG")) -- aborting" && exit 1)
    }

    function load_kv_config {
      export NODE_CONFIG="$DATA_DIR/config/custom/config.json"
      export DB_SYNC_CONFIG="$DATA_DIR/config/custom/db-sync-config.json"
      export SHELLEY_GENESIS_FILE="shelley-genesis.json"
      export BYRON_GENESIS_FILE="byron-genesis.json"
      export ALONZO_GENESIS_FILE="alonzo-genesis.json"

      [ -z "''${CONSUL_KV_PATH:-}" ] && echo "CONSUL_KV_PATH env var must be set -- aborting" && exit 1
      [ -z "''${CONSUL_HTTP_ADDR:-}" ] && echo "CONSUL_HTTP_ADDR env var must be set -- aborting" && exit 1
      [ -z "''${CONSUL_HTTP_TOKEN:-}" ] && echo "CONSUL_HTTP_TOKEN env var must be set -- aborting" && exit 1

      [ -z "''${WORKLOAD_CACERT:-}" ] && echo "WORKLOAD_CACERT env var must be set -- aborting" && exit 1
      [ -z "''${WORKLOAD_CLIENT_CERT:-}" ] && echo "WORKLOAD_CLIENT_CERT env var must be set -- aborting" && exit 1
      [ -z "''${WORKLOAD_CLIENT_KEY:-}" ] && echo "WORKLOAD_CLIENT_KEY env var must be set -- aborting" && exit 1
      export CONSUL_CACERT="$WORKLOAD_CACERT"
      export CONSUL_CLIENT_CERT="$WORKLOAD_CLIENT_CERT"
      export CONSUL_CLIENT_KEY="$WORKLOAD_CLIENT_KEY"

      local cmd=(
        "curl"
        "$CONSUL_HTTP_ADDR/v1/kv/$CONSUL_KV_PATH?raw"
        "--header" "X-Consul-Token: $CONSUL_HTTP_TOKEN"
        "--header" "Content-Type: application/json"
      )

      local json
      json=$("''${cmd[@]}")

      echo "$json"|jq '.nodeConfig'  > "$NODE_CONFIG"
      echo "$json"|jq '.dbSyncConfig'  > "$DB_SYNC_CONFIG"

      echo "$json"|jq -r '.byronGenesisBlob'  |base64 -d > "$DATA_DIR/config/custom/$BYRON_GENESIS_FILE"
      echo "$json"|jq -r '.shelleyGenesisBlob'|base64 -d > "$DATA_DIR/config/custom/$SHELLEY_GENESIS_FILE"
      # alegra
      # mary
      echo "$json"|jq -r '.alonzoGenesisBlob' |base64 -d > "$DATA_DIR/config/custom/$ALONZO_GENESIS_FILE"
      # vasil

      # ensure genesis file contracts
      ensure_file_location_contract "ShelleyGenesisFile" "$SHELLEY_GENESIS_FILE"
      ensure_file_location_contract "ByronGenesisFile" "$BYRON_GENESIS_FILE"
      ensure_file_location_contract "AlonzoGenesisFile" "$ALONZO_GENESIS_FILE"
    }

    function load_kv_secrets {
      export BYRON_DELEG_CERT=/secrets/byron_deleg_cert.cert
      export BYRON_SIGNING_KEY=/secrets/byron_signing_key.key
      export SHELLEY_KES_KEY=/secrets/shelley_kes_key.skey
      export SHELLEY_VRF_KEY=/secrets/shelley_vrf_key.skey
      export SHELLEY_OPCERT=/secrets/shelley_opcert.opscert

      [ -z "''${VAULT_KV_PATH:-}" ] && echo "VAULT_KV_PATH env var must be set -- aborting" && exit 1
      [ -z "''${VAULT_ADDR:-}" ] && echo "VAULT_ADDR env var must be set -- aborting" && exit 1
      [ -z "''${VAULT_TOKEN:-}" ] && echo "VAULT_TOKEN env var must be set -- aborting" && exit 1

      [ -z "''${WORKLOAD_CACERT:-}" ] && echo "WORKLOAD_CACERT env var must be set -- aborting" && exit 1
      [ -z "''${WORKLOAD_CLIENT_CERT:-}" ] && echo "WORKLOAD_CLIENT_CERT env var must be set -- aborting" && exit 1
      [ -z "''${WORKLOAD_CLIENT_KEY:-}" ] && echo "WORKLOAD_CLIENT_KEY env var must be set -- aborting" && exit 1

      export  VAULT_CACERT="$WORKLOAD_CACERT"
      export  VAULT_CLIENT_CERT="$WORKLOAD_CLIENT_CERT"
      export  VAULT_CLIENT_KEY="$WORKLOAD_CLIENT_KEY"

      local cmd=(
        "curl"
        "$VAULT_ADDR/v1/$VAULT_KV_PATH"
        "--header" "X-Vault-Token: $VAULT_TOKEN"
        "--header" "Content-Type: application/json"
      )

      local json
      json=$("''${cmd[@]}" | jq '.data.data')

      echo "$json"|jq -e '."byron.cert.json"'  > "$BYRON_DELEG_CERT" || unset BYRON_DELEG_CERT BYRON_SIGNING_KEY
      # we only want to fetch and set cold key if byron certificacte is passed to the node
      if [ -n "''${BYRON_DELEG_CERT:-}" ]; then
        # we use the shelley delegate as transport because it's already encoded for transport. Here we extract and decode to it's byron era bin format.
        echo "$json" | jq -e -r '."cold.skey".cborHex' | xxd -r -p - > "$BYRON_SIGNING_KEY" || unset BYRON_SIGNING_KEY
      fi
      echo "$json"|jq -e '."kes.skey"'    > "$SHELLEY_KES_KEY" || unset SHELLEY_KES_KEY
      echo "$json"|jq -e '."vrf.skey"'    > "$SHELLEY_VRF_KEY" || unset SHELLEY_VRF_KEY
      echo "$json"|jq -e '."opcert.json"' > "$SHELLEY_OPCERT"  || unset SHELLEY_OPCERT

      test -z "''${BYRON_DELEG_CERT:-}"  || chmod 0400 "$BYRON_DELEG_CERT"
      test -z "''${BYRON_SIGNING_KEY:-}" || chmod 0400 "$BYRON_SIGNING_KEY"
      test -z "''${SHELLEY_KES_KEY:-}"   || chmod 0400 "$SHELLEY_KES_KEY"
      test -z "''${SHELLEY_VRF_KEY:-}"   || chmod 0400 "$SHELLEY_VRF_KEY"
      test -z "''${SHELLEY_OPCERT:-}"    || chmod 0400 "$SHELLEY_OPCERT"
    }
  '';

  legacy-srv-discovery = ''
    function watch_srv_discovery {
      declare -i pid_to_signal=$1
      while true
      do
        sleep 30
        echo "Service discovery heartbeat - every 30 seconds" > /dev/stderr
        original_hash="$(md5sum "$NODE_TOPOLOGY")"
        srv_discovery
        new_hash="$(md5sum "$NODE_TOPOLOGY")"
        [ "$original_hash" != "$new_hash" ] && kill -1 "$pid_to_signal"
      done
    }

    function srv_discovery {

      export NODE_TOPOLOGY="$DATA_DIR/config/custom/topology.json"

      # general values
      # TODO: make this part of the kv-config, etc
      echo '{}' | jq '{
        useLedgerAfterSlot: 86400
      }' > ./topology-common.json

      # public roots -> SIGHUP
      # this contains also the "self" root, else we would need to filter that out
      # and that would be _ugly_
      (srvaddr -json publics="$PUBLIC_ROOTS_SRV_DNS" | jq '{
        PublicRoots: [
          { publicRoots: {
            accessPoints: .[] | map({address: .Host, port: .Port}),
            advertise: false
          } }
        ]
      }' || echo '{ "PublicRoots": [{
        "publicRoots": {
          "accessPoints": [
          ],
          "advertise": false
        }
      }] }') > ./topology-publics.json

      # local roots -> SIGHUP
      (srvaddr -json locals="$LOCAL_ROOTS_SRV_DNS" | jq '{
        LocalRoots: {
          groups: [
            {
              localRoots: {
                accessPoints: .[] | map({address: .Host, port: .Port}),
                advertise: false
              },
              valency: 1
            }
          ]
        }
      }' || echo '{ "LocalRoots": {
        "groups": [
          {
            "localRoots": {
              "accessPoints": [
              ],
              "advertise": false
            },
            "valency": 1
          }
        ]
      } }') > ./topology-locals.json

      # construe topology
      cat \
        ./topology-common.json \
        ./topology-locals.json \
        ./topology-publics.json \
        | jq -s 'add' > "$NODE_TOPOLOGY"
    }
  '';
in {
  cardano-node = writeShellApplication {
    name = "entrypoint";
    runtimeInputs = prelude-runtime ++ pull-snapshot-deps;
    debugInputs = [packages.cardano-cli packages.cardano-node];
    text = ''

      # in nomad: producer is always the node with index 0
      producer=0
      [ "''${NOMAD_ALLOC_INDEX:-1}" -eq "0" ] && [ -z "''${EDGE_NODE:-}" ] && producer=1

      ${prelude}

      DB_DIR="$DATA_DIR/db-''${ENVIRONMENT:-custom}"

      # the legacy service discovery implementation
      ${legacy-srv-discovery}

      # Grap a snapshot
      ${pull-snapshot}
      if [ -n "''${SNAPSHOT_BASE_URL:-}" ]; then
        pull_snapshot
        extract_snapshot_tgz_to "$DB_DIR/node"
      fi

      # Build args array
      args+=("--config" "$NODE_CONFIG")
      args+=("--database-path" "$DB_DIR/node")
      [ -n "''${HOST_ADDR:-}" ] && args+=("--host-addr" "$HOST_ADDR")
      [ -n "''${HOST_IPV6_ADDR:-}" ] && args+=("--host-ipv6-addr" "$HOST_IPV6_ADDR")
      [ -n "''${PORT:-}" ] && args+=("--port" "$PORT")
      [ -n "''${SOCKET_PATH:-}" ] && args+=("--socket-path" "$SOCKET_PATH")

      # Ignore RTS flags for now. Need to figure out best way to pass a list
      #[ -n "''${RTS_FLAGS:-}" ] && args+=$RTS_FLAGS

      [ -n "''${BYRON_DELEG_CERT:-}" ] && args+=("--byron-delegation-certificate" "$BYRON_DELEG_CERT")
      [ -n "''${BYRON_SIGNING_KEY:-}" ] && args+=("--byron-signing-key" "$BYRON_SIGNING_KEY")
      [ -n "''${SHELLEY_KES_KEY:-}" ] && args+=("--shelley-kes-key" "$SHELLEY_KES_KEY")
      [ -n "''${SHELLEY_VRF_KEY:-}" ] && args+=("--shelley-vrf-key" "$SHELLEY_VRF_KEY")
      [ -n "''${SHELLEY_OPCERT:-}" ] && args+=("--shelley-operational-certificate" "$SHELLEY_OPCERT")

      if [ -z "''${NODE_TOPOLOGY:-}" ]; then
        echo "Doing legacy service discovery ..." > /dev/stderr
        srv_discovery
        args+=("--topology" "$NODE_TOPOLOGY")

        # turn on bash's job control
        set -m

        # Define handler for SIGINT
        trap "kill" "''${sid[@]}" INT

        # SIGHUP reloads --topology
        echo Running node in background > /dev/stderr
        ${packages.cardano-node}/bin/cardano-node run "''${args[@]}" &
        CARDANO_PID="$!"
        sid=("$CARDANO_PID")
        echo Running service discovery loop > /dev/stderr
        watch_srv_discovery "$CARDANO_PID"
      else
        [ -z "''${NODE_TOPOLOGY:-}" ] && echo "NODE_TOPOLOGY env var must be set -- aborting" && exit 1
        args+=("--topology" "$NODE_TOPOLOGY")
        exec ${packages.cardano-node}/bin/cardano-node run "''${args[@]}"
      fi
    '';
  };

  cardano-tracer = writeShellApplication {
    runtimeInputs = [nixpkgs.coreutils nixpkgs.jq];
    debugInputs = [packages.cardano-tracer];
    name = "entrypoint";
    text = ''
      args+("")
      exec ${packages.cardano-tracer}/bin/cardano-tracer run "''${args[@]}"
    '';
  };

  cardano-db-sync = writeShellApplication {
    runtimeInputs = prelude-runtime ++ pull-snapshot-deps ++ [nixpkgs.postgresql_12];
    debugInputs = [
      packages.cardano-db-sync
      packages.cardano-cli
      nixpkgs.strace # for debugging libq
    ];
    name = "entrypoint";
    text = ''

      ${prelude}

      mkdir -m 1777 /tmp

      function watch_leader_discovery {
        declare -i pid_to_signal=$1
        while true
        do
          sleep 15
          echo "Service discovery heartbeat - every 15 seconds" > /dev/stderr
          original_addr="$PSQL_ADDR0"
          leader_discovery
          new_addr="$PSQL_ADDR0"
          # load_kv_secrets_db_sync
          [ "$original_addr" != "$new_addr" ] && kill -1 "$pid_to_signal"
        done
      }

      function leader_discovery {
        eval "$(srvaddr -env PSQL="$MASTER_REPLICA_SRV_DNS")"
        # produces:
        # PSQL_ADDR0=domain:port
        # PSQL_HOST0=domain
        # PSQL_PORT0=port

        export PGPASSFILE=/secrets/pgpass
        export PSQL_ADDR0

        echo "Retrieving db credentials from vault kv ..." > /dev/stderr
        load_kv_secrets_db_sync
      }

      ${legacy-kv-config-instrumentation-db-sync}

      if [ -n "''${VAULT_KV_PATH:-}" ]; then
        [ -z "''${MASTER_REPLICA_SRV_DNS:-}" ] && echo "MASTER_REPLICA_SRV_DNS env var must be set -- aborting" && exit 1
        [ -z "''${DB_NAME:-}" ] && echo "DB_NAME env var must be set -- aborting" && exit 1
        echo "Retrieving db leader from $MASTER_REPLICA_SRV_DNS ..." > /dev/stderr
        leader_discovery
      fi

      DB_DIR="$DATA_DIR/db-''${ENVIRONMENT:-custom}"

      [ -z "''${PGPASSFILE:-}" ] && echo "PGPASSFILE env var must be set -- aborting" && exit 1
      [ -z "''${SOCKET_PATH:-}" ] && echo "SOCKET_PATH env var must be set -- aborting" && exit 1

      # Grap a snapshot
      ${pull-snapshot}
      if [ -n "''${SNAPSHOT_BASE_URL:-}" ]; then
        pull_snapshot
        extract_snapshot_tgz_to "$DB_DIR/db-sync"
      fi

      # Build args array
      args+=("--config" "$DB_SYNC_CONFIG")
      args+=("--socket-path" "$SOCKET_PATH")
      args+=("--state-dir" "$DB_DIR/db-sync")
      args+=("--schema-dir" "${inputs.cardano-db-sync + "/schema"}")


      if [ -n "''${MASTER_REPLICA_SRV_DNS:-}" ]; then
        echo "Doing legacy leader discovery ..." > /dev/stderr

        # turn on bash's job control
        set -m

        # Define handler for SIGINT
        trap "kill" "''${sid[@]}" INT

        # SIGHUP reloads --topology
        echo Running db-sync in background > /dev/stderr
        ${packages.cardano-db-sync}/bin/cardano-db-sync "''${args[@]}" &
        DB_SYNC_PID="$!"
        sid=("$DB_SYNC_PID")
        echo Running leader discovery loop > /dev/stderr
        watch_leader_discovery "$DB_SYNC_PID"
      else
        exec ${packages.cardano-db-sync}/bin/cardano-db-sync "''${args[@]}"
      fi
    '';
  };

  cardano-wallet = writeShellApplication {
    runtimeInputs = prelude-runtime;
    debugInputs = [packages.cardano-wallet packages.cardano-cli];
    name = "entrypoint";
    text = ''

      ${prelude}
      DB_SYNC_CONFIG="$DATA_DIR/config/$ENVIRONMENT/db-sync-config.json"

      DB_DIR="$DATA_DIR/db-''${ENVIRONMENT:-custom}"

      # Build args array
      args+=("--listen-address" "0.0.0.0")
      args+=("--port" "8090")
      args+=("--node-socket" "$SOCKET_PATH")
      args+=("--database" "$DB_DIR/wallet")
      # FIXME: consume the node config directly
      args+=("$(
        [ "''${ENVIRONMENT}" == "mainnet" ] &&
          echo "--mainnet" ||
          echo "--testnet-magic $(
          jq '.networkMagic' "$(
            file="$(jq '.ShelleyGenesisFile' "$NODE_CONFIG" )"
            folder="$(dirname "$NODE_CONFIG")"
            [[ "$file" == /* ]] && echo "$file" || echo "$folder/$file"
          )"
        )"
      )")

      # Wallet will not export prometheus metrics without also enabling EKG
      export CARDANO_WALLET_EKG_HOST=127.0.0.1
      export CARDANO_WALLET_EKG_PORT=8083
      export CARDANO_WALLET_PROMETHEUS_HOST=127.0.0.1
      export CARDANO_WALLET_PROMETHEUS_PORT=8082

      exec ${packages.cardano-wallet}/bin/cardano-wallet serve "''${args[@]}"
    '';
  };

  cardano-submit-api = writeShellApplication {
    runtimeInputs = prelude-runtime;
    debugInputs = [packages.cardano-submit-api packages.cardano-cli];
    name = "entrypoint";
    text = ''

      ${prelude}

      # Build args array
      args+=("--listen-address" "0.0.0.0")
      args+=("--port" "8070")
      args+=("--config" "$NODE_CONFIG")

      exec ${packages.cardano-submit-api}/bin/cardano-submit-api "''${args[@]}"
    '';
  };

  ogmios = writeShellApplication {
    runtimeInputs = prelude-runtime;
    debugInputs = [packages.ogmios];
    name = "entrypoint";
    text = ''

      ${prelude}

      # Build args array
      args+=("--host" "0.0.0.0")
      args+=("--port" "8070")
      args+=("--node-socket" "$SOCKET_PATH")
      args+=("--node-config" "$NODE_CONFIG")

      exec ${packages.ogmios}/bin/ogmios "''${args[@]}"
    '';
  };
}
