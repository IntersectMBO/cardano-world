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
    # Exit if any required variables are not set
    [ -z "''${SOCKET_PATH:-}" ] && echo "SOCKET_PATH env var must be set -- aborting" && exit 1

    ${config-data.copyEnvsTemplate config-data.environments}
    if [ -n "''${ENVIRONMENT:-}" ]
    then
      NODE_CONFIG="$DATA_DIR/config/$ENVIRONMENT/config.json"
    fi

    # TODO: Need more logic here if config/topology are not set e.g. mainnet/testnet
    [ -z "''${NODE_CONFIG:-}" ] && echo "NODE_CONFIG env var must be set -- aborting" && exit 1

    CARDANO_CLI_ENV_FLAG="$(
      [ "''${ENVIRONMENT}" == "mainnet" ]
      && echo "--mainnet"
      || echo "--testnet-magic $(
        cat $(cat "$NODE_CONFIG" | jq '.ShelleyGenesisFile' )
        | jq '.networkMagic'
      )"
    )"

    NODE_STATUS="$(env CARDANO_NODE_SOCKET_PATH="$SOCKET_PATH" cardano-cli query tip "$CARDANO_CLI_ENV_FLAG" 2>/dev/null || :)"

    echo "Cardano node status:"
    jq <<<"$NODE_STATUS" || :


  '';
in {
  cardano-node-network-sync = writeShellApplication {
    runtimeInputs = [packages.cardano-cli nixpkgs.jq nixpkgs.coreutils];
    name = "healthcheck";
    text = ''

      ${prelude}

      jq -e '.syncProgress == "100.00"' <<<"$NODE_STATUS" || exit 1
    '';
  };
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
  cardano-db-sync-network-sync = writeShellApplication {
    runtimeInputs = [packages.cardano-cli nixpkgs.jq nixpkgs.coreutils];
    name = "healthcheck";
    text = ''

      ${prelude}

      NODE_BLOCK_HEIGHT="$(jq -e -r '.block' <<<"$NODE_STATUS" 2>/dev/null || :)"

      DB_STATUS="$(curl -s localhost:8080 2>/dev/null | grep -v '#' || :)"
      DB_BLOCK_HEIGHT="$(echo "$DB_STATUS" | grep -oP '^cardano_db_sync_db_block_height\s+\K[0-9]+' || :)"

      echo "Cardano db sync status:"
      echo "$DB_STATUS" || :
      echo
      echo "Compare node to db blockHeight: ($NODE_BLOCK_HEIGHT, $DB_BLOCK_HEIGHT)"

      # Failure modes:
      [ -z "$NODE_BLOCK_HEIGHT" ] && [ -z "$DB_BLOCK_HEIGHT" ] && exit 1

      # Exits as a warning if DB Sync is more than 10 blocks behind node
      [ $(("$NODE_BLOCK_HEIGHT" - "$DB_BLOCK_HEIGHT")) -le 10 ] || exit 1

    '';
  };
}
