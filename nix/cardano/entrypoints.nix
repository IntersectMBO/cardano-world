{
  inputs,
  cell,
}: let
  inherit (inputs) nixpkgs;
  inherit (inputs.bitte-cells._writers.library) writeShellApplication;
  inherit (inputs.bitte-cells._utils.packages) srvaddr;
  inherit (cell) packages;
  inherit (cell) config-data;

  prelude = ''
    [ -z "''${DATA_DIR:-}" ] && echo "DATA_DIR env var must be set -- aborting" && exit 1

    mkdir -p "$DATA_DIR"
    DB_DIR="$DATA_DIR/db"
    mkdir -p "$DATA_DIR/config"
    chmod -R +w "$DATA_DIR/config"

    # the menu of environments that we ship as built-in envs
    ${config-data.copyEnvsTemplate config-data.environments}

    # the legacy implementation to access kv config
    ${legacy-kv-config-instrumentation}

    # the legacy service discovery implementation
    ${legacy-srv-discovery}

    # CASE: built-in environment
    if [ -n "''${ENVIRONMENT:-}" ]; then

      NODE_CONFIG="$DATA_DIR/config/$ENVIRONMENT/config.json"
      NODE_TOPOLOGY="$DATA_DIR/config/$ENVIRONMENT/topology.json"
      DB_DIR="$DATA_DIR/db-$ENVIRONMENT"

    # CASE: premissioned long running environment
    elif [ -n "''${CONSUL_KV_PATH:-}" || -n "''${VAULT_KV_PATH:-}" ]

      load_kv_config
      [ "$producer" == "1"] && load_kv_secrets

      if [ -n "''${NODE_TOPOLOGY:-}" ]; then; :; else
        global srv_discovery=1
        srv_discovery
      fi

    # CASE: permissioned short running environment
    else

      [ -z "''${NODE_CONFIG:-}" ] && echo "NODE_CONFIG env var must be set -- aborting" && exit 1

      if [ -n "''${NODE_TOPOLOGY:-}" ]; then; :; else
        global srv_discovery=1
        srv_discovery
      fi

    fi
  '';

  legacy-kv-config-instrumentation = ''
    func ensure_file_location_contract {
      local key="$1"
      lecal file="$2"
      [ jq -e --arg KEY $key --arg FILE $file '.[$KEY] == $FILE' ] && echo "$file is not located where it needs to be -- aborting" && exit 1
    }

    func load_kv_config {
      export NODE_CONFIG="$DATA_DIR/config/custom/config.json"
      export SHELLEY_GENESIS_FILE=./shelley-genesis-file.json"
      export BYRON_GENESIS_FILE=./shelley-genesis-file.json"
      export ALONZO_GENESIS_FILE=./alonzo-genesis-file.json"

      [ -z "''${CONSUL_KV_PATH:-}" ] && echo "CONSUL_KV_PATH env var must be set -- aborting" && exit 1
      [ -z "''${CONSUL_HTTP_ADDR:-}" ] && echo "CONSUL_HTTP_ADDR env var must be set -- aborting" && exit 1
      [ -z "''${CONSUL_HTTP_TOKEN:-}" ] && echo "CONSUL_HTTP_TOKEN env var must be set -- aborting" && exit 1

      [ -z "''${WORKLOAD_CACERT:-}" ] && echo "WORKLOAD_CACERT env var must be set -- aborting" && exit 1
      [ -z "''${WORKLOAD_CLIENT_CERT:-}" ] && echo "WORKLOAD_CLIENT_CERT env var must be set -- aborting" && exit 1
      [ -z "''${WORKLOAD_CLIENT_KEY:-}" ] && echo "WORKLOAD_CLIENT_KEY env var must be set -- aborting" && exit 1
      export CONSUL_CACERT="$WORKLOAD_CACERT"
      export CONSUL_CLIENT_CERT="$WORKLOAD_CLIENT_CERT"
      export CONSUL_CLIENT_KEY="$WORKLOAD_CLIENT_KEY"

      consul kv get "$CONSUL_KV_PATH"|jq '.config'  > "$NODE_CONFIG"

      consul kv get "$CONSUL_KV_PATH"|jq '.byron-genesis'  > "$DATA_DIR/config/custom/$BYRON_GENESIS_FILE"
      consul kv get "$CONSUL_KV_PATH"|jq '.shelley-genesis'  > "$DATA_DIR/config/custom/$SHELLEY_GENESIS_FILE"
      # alegra
      # mary
      consul kv get "$CONSUL_KV_PATH"|jq '.alonzo-genesis'  > "$DATA_DIR/config/custom/$ALONZO_GENESIS_FILE"
      # vasil

      # ensure genisis file contracts
      ensure_file_location_contract ".ShelleyGenesisFile" "$SHELLEY_GENESIS_FILE"
      ensure_file_location_contract ".ByronGenesisFile" "$BYRON_GENESIS_FILE"
      ensure_file_location_contract ".AlonzoGenesisFile" "$ALONZO_GENESIS_FILE"
    }

    func load_kv_secrets {
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

      vault kv get "$VAULT_KV_PATH"|jq '.byron_deleg_cert'  > "$BYRON_DELEG_CERT"
      vault kv get "$VAULT_KV_PATH"|jq '.byron_signing_key'  > "$BYRON_SIGNING_KEY"
      vault kv get "$VAULT_KV_PATH"|jq '.shelley_kes_key'  > "$SHELLEY_KES_KEY"
      vault kv get "$VAULT_KV_PATH"|jq '.shelley_vrf_key'  > "$SHELLEY_VRF_KEY"
      vault kv get "$VAULT_KV_PATH"|jq '.shelley_opcert'  > "$SHELLEY_OPCERT"
    }
  '';

  legacy-srv-discovery = ''
    func watch_srv_discovery {
      declare -i pid_to_signal=$1
      while true
      do
        sleep 30
        original_hash="$(md5sum "$NODE_TOPOLOGY")"
        srv_discovery
        new_hash="$(md5sum "$NODE_TOPOLOGY")"
        [ "$original_hash" != "$new_hash" ] && kill -1 $pid_to_signal
      done
    }

    func srv_discovery {

      [ -z "''${LOCAL_ROOTS_SRV_DNS:-}" ] && echo "LOCAL_ROOTS_SRV_DNS env var must be set -- aborting" && exit 1
      [ -z "''${PUBLIC_ROOTS_SRV_DNS:-}" ] && echo "PUBLIC_ROOTS_SRV_DNS env var must be set -- aborting" && exit 1

      export NODE_TOPOLOGY="$DATA_DIR/config/custom/topology.json"

      # general values
      # TODO: make this part of the kv-config, etc
      echo '{}' | jq '{
        useLedgerAfterSlot: 7200
      }' > ./topology-common.json

      # local roots -> SIGHUP
      srvaddr -json locals=$LOCAL_ROOTS_SRV_DNS | jq '{
        LocalRoots: {
          groups: [
            {
              localRoots: {
                accessPoints: .[] | map({address: .Host, port: .Port})
                advertise: false
              }
              valency: 1
            }
          ]
        }
      }' > ./topology-locals.json

      # public roots -> SIGHUP
      # this contains also the "self" root, else we would need to filter that out
      # and that would be _ugly_
      srvaddr -json publics=$PUBLIC_ROOTS_SRV_DNS | jq '{
        PublicRoots: [
          { publicRoots: {
            accessPoints: .[] | map({address: .Host, port: .Port})
            advertise: false
          } }
        ]
      }' > ./topology-publics.json

      # construe topology
      cat \ # the p2p case
        ./topology-common.json \
        ./topology-locals.json \
        ./topology-publics.json \
        | jq -s 'add' > "$NODE_TOPOLOGY"
    }
  '';
in {
  cardano-node = writeShellApplication {
    name = "entrypoint";
    runtimeInputs = [nixpkgs.consul nixpkgs.vault nixpkgs.jq];
    text = ''

      # in nomad: producer is always the node with index 0
      producer="$(! test "0" -eq "''${NOMAD_ALLOC_ID:-}")$?"

      ${prelude}

      # Build args array
      args+=("--config" "$NODE_CONFIG")
      args+=("--database-path" "$DB_DIR/node")
      args+=("--topology" "$NODE_TOPOLOGY")
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

      if [ "''${srv_discovery}" == "1"]; then

        # turn on bash's job control
        set -m

        # Define handler for SIGINT
        trap "kill ''${sid[@]}" INT

        # SIGHUP reloads --topology
        ${packages.cardano-node}/bin/cardano-node run "''${args[@]}" &
        sid=($!)

        watch_srv_discovery $! &
        sid=+($!)

        # foreground the cardano-node process
        fg %1
      else
        exec ${packages.cardano-node}/bin/cardano-node run "''${args[@]}"
      fi
    '';
  };

  cardano-db-sync = writeShellApplication {
    runtimeInputs = [nixpkgs.coreutils nixpkgs.jq];
    name = "entrypoint";
    text = ''

      NODE_TOPOLOGY="do-not-load"
      ${prelude}

      [ -z "''${PGPASSFILE:-}" ] && echo "PGPASSFILE env var must be set -- aborting" && exit 1
      [ -z "''${SOCKET_PATH:-}" ] && echo "SOCKET_PATH env var must be set -- aborting" && exit 1

      # Permission the PGPASSFILE
      cp "''${PGPASSFILE}" "''${PGPASSFILE}.permissioned"
      chmod 600 "''${PGPASSFILE}.permissioned"
      export PGPASSFILE="''${PGPASSFILE}.permissioned"

      # Build args array
      args+=("--config" "$NODE_CONFIG")
      args+=("--socket-path" "$SOCKET_PATH")
      args+=("--state-dir" "$DB_DIR/db-sync")
      args+=("--schema-dir" "${inputs.cardano-db-sync + "/schema"})

      exec ${packages.cardano-db-sync}/bin/cardano-db-sync run "''${args[@]}"
    '';
  };

  cardano-wallet = writeShellApplication {
    runtimeInputs = [nixpkgs.coreutils nixpkgs.jq];
    name = "entrypoint";
    text = ''

      NODE_TOPOLOGY="do-not-load"
      ${prelude}

      # Build args array
      args+=("--listen-address" "0.0.0.0")
      args+=("--port" "8090")
      args+=("--node-socket" "$SOCKET_PATH")
      args+=("--database" "$DB_DIR/wallet")
      # FIXME: consume the node config directly
      args+=("$(
        [ "''${ENVIRONMENT}" == "mainnet" ]
        && echo "--mainnet"
        || echo "--testnet-magic $(
          cat "$(
            file="$(cat "$NODE_CONFIG" | jq '.ShelleyGenesisFile' )"
            folder="$(dirname $NODE_CONFIG)"
            [[ "$file" == /* ]] && echo "$file" || echo "$folder/$file"
          )"
          | jq '.networkMagic'
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
    runtimeInputs = [nixpkgs.coreutils nixpkgs.jq];
    name = "entrypoint";
    text = ''

      NODE_TOPOLOGY="do-not-load"
      ${prelude}

      # Build args array
      args+=("--listen-address" "0.0.0.0")
      args+=("--port" "8070")
      args+=("--config" "$NODE_CONFIG")

      exec ${packages.cardano-submit-api}/bin/cardano-submit-api "''${args[@]}"
    '';
  };
}
