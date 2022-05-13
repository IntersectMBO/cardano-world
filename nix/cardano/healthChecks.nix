{
  inputs,
  cell,
}: let
  inherit (inputs) nixpkgs;
  inherit (cell) packages library nixosProfiles;
  inherit (inputs.bitte-cells._writers.library) writeShellApplication;
  inherit (inputs.bitte-cells._utils.packages) srvaddr;
  inherit (inputs.nixpkgs.lib.strings) fileContents;

  prelude = ''
    [ -z "''${SOCKET_PATH:-}" ] && echo "SOCKET_PATH env var must be set -- aborting" && exit 1
    [ -z "''${DATA_DIR:-}" ] && echo "DATA_DIR env var must be set -- aborting" && exit 1

    mkdir -p "$DATA_DIR"
    DB_DIR="$DATA_DIR/db"
    mkdir -p "$DATA_DIR/config"
    chmod -R +w "$DATA_DIR/config"

    # the menu of environments that we ship as built-in envs
    ${config-data.copyEnvsTemplate config-data.environments}

    # CASE: built-in environment
    if [ -n "''${ENVIRONMENT:-}" ]; then

      NODE_CONFIG="$DATA_DIR/config/$ENVIRONMENT/config.json"

    # CASE: premissioned long running environment
    elif [ -n "''${CONSUL_KV_PATH:-}" ]

      export NODE_CONFIG="$DATA_DIR/config/custom/config.json"
      export SHELLEY_GENESIS_FILE=./shelley-genesis-file.json"

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
      consul kv get "$CONSUL_KV_PATH"|jq '.shelley-genesis'  > "$DATA_DIR/config/custom/$SHELLEY_GENESIS_FILE"

    # CASE: permissioned short running environment
    else

      [ -z "''${NODE_CONFIG:-}" ] && echo "NODE_CONFIG env var must be set -- aborting" && exit 1

    fi

    CARDANO_CLI_ENV_FLAG="$(
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
    )"

    NODE_STATUS="$(env CARDANO_NODE_SOCKET_PATH="$SOCKET_PATH" cardano-cli query tip "$CARDANO_CLI_ENV_FLAG" 2>/dev/null || :)"

    echo "Cardano node status:"
    jq <<<"$NODE_STATUS" || :


  '';
in {
  cardano-wallet-network-sync = writeShellApplication {
    runtimeInputs = [srvaddr nixpkgs.jq nixpkgs.coreutils];
    name = "healthcheck";
    text = ''
      #!/bin/bash

      [ -z "${WALLET_SRV_FQDN:-}" ] && echo "WALLET_SRV_FQDN env var must be set -- aborting" && exit 1
      [ -z "${CARDANO_WALLET_ID:-}" ] && echo "CARDANO_WALLET_ID env var must be set -- aborting" && exit 1

      mapfile -t wallet_urls <<<"$(srvaddr "${WALLET_SRV_FQDN}")"

      STATUS="$(curl -sf "${wallet_urls [0]}/v2/wallets/$CARDANO_WALLET_ID" || :)"
      jq <<<"$STATUS" || :
      jq -e '.state.status == "ready"' <<<"$STATUS" || exit 1
    '';
  };
  cardano-submit-api-network-sync = writeShellApplication {
    runtimeInputs = [srvaddr nixpkgs.jq nixpkgs.coreutils];
    name = "healthcheck";
    text = ''
      #!/bin/bash

      [ -z "${WALLET_SRV_FQDN:-}" ] && echo "WALLET_SRV_FQDN env var must be set -- aborting" && exit 1
      [ -z "${CARDANO_WALLET_ID:-}" ] && echo "CARDANO_WALLET_ID env var must be set -- aborting" && exit 1

      mapfile -t wallet_urls <<<"$(srvaddr "${WALLET_SRV_FQDN}")"

      STATUS="$(curl -sf "${wallet_urls [0]}/v2/wallets/$CARDANO_WALLET_ID" || :)"
      jq <<<"$STATUS" || :
      jq -e '.state.status == "ready"' <<<"$STATUS" || exit 1
    '';
  };
}
