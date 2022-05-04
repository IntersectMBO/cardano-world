{
  inputs,
  cell,
}: let
  inherit (inputs) nixpkgs;
  inherit (inputs.bitte-cells._writers.library) writeShellApplication;
  inherit (cell) packages;
  inherit (cell) config-data;

  prelude = ''
    # Exit if any required variables are not set
    [ -z "''${DATA_DIR:-}" ] && echo "DATA_DIR env var must be set -- aborting" && exit 1

    mkdir -p "$DATA_DIR"
    DB_DIR="$DATA_DIR/db"
    mkdir -p "$DATA_DIR/config"
    chmod -R +w "$DATA_DIR/config"
    ${config-data.copyEnvsTemplate config-data.environments}
    if [ -n "''${ENVIRONMENT:-}" ]
    then
      NODE_CONFIG="$DATA_DIR/config/$ENVIRONMENT/config.json"
      NODE_TOPOLOGY="$DATA_DIR/config/$ENVIRONMENT/topology.json"
      DB_DIR="$DATA_DIR/db-$ENVIRONMENT"
    fi

    # TODO: Need more logic here if config/topology are not set e.g. mainnet/testnet
    [ -z "''${NODE_CONFIG:-}" ] && echo "NODE_CONFIG env var must be set -- aborting" && exit 1
    [ -z "''${NODE_TOPOLOGY:-}" ] && echo "NODE_TOPOLOGY env var must be set -- aborting" && exit 1
  '';
in {
  cardano-node = writeShellApplication {
    name = "entrypoint";
    text = ''

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

      exec ${packages.cardano-node}/bin/cardano-node run "''${args[@]}"
    '';
  };

  cardano-db-sync = writeShellApplication {
    runtimeInputs = [nixpkgs.coreutils nixpkgs.jq];
    name = "entrypoint";
    text = ''

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
          cat $(cat "$NODE_CONFIG" | jq '.ShelleyGenesisFile' )
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

      ${prelude}

      # Build args array
      args+=("--listen-address" "0.0.0.0")
      args+=("--port" "8070")
      args+=("--config" "$NODE_CONFIG")

      exec ${packages.cardano-submit-api}/bin/cardano-submit-api "''${args[@]}"
    '';
  };
}
