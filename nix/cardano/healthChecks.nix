{
  inputs,
  cell,
}: let
  inherit (inputs) nixpkgs;
  inherit (inputs.bitte-cells._writers.library) writeShellApplication;
  inherit (inputs.bitte-cells._utils.packages) srvaddr;
  inherit (cell) environments library packages;
in {
  cardano-wallet-network-sync = writeShellApplication {
    runtimeInputs = [srvaddr nixpkgs.jq nixpkgs.coreutils nixpkgs.curl];
    name = "healthcheck";
    text = ''
      #!/bin/bash

      [ -z "''${WALLET_SRV_FQDN:-}" ] && echo "WALLET_SRV_FQDN env var must be set -- aborting" && exit 1
      [ -z "''${CARDANO_WALLET_ID:-}" ] && echo "CARDANO_WALLET_ID env var must be set -- aborting" && exit 1

      mapfile -t wallet_urls <<<"$(srvaddr "''${WALLET_SRV_FQDN}")"

      STATUS="$(curl -sf "''${wallet_urls[0]}/v2/wallets/$CARDANO_WALLET_ID" || :)"
      jq <<<"$STATUS" || :
      jq -e '.state.status == "ready"' <<<"$STATUS" || exit 1
    '';
  };
  cardano-submit-api-network-sync = writeShellApplication {
    runtimeInputs = [packages.cardano-cli nixpkgs.jq nixpkgs.coreutils];
    name = "healthcheck";
    text = ''
      ENV_FLAG=("$(
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
      NODE_STATUS="$(env CARDANO_NODE_SOCKET_PATH="$SOCKET_PATH" cardano-cli query tip "''${ENV_FLAG[@]}" 2>/dev/null || true)"
      SYNC_PROGRESS="$(jq -e -r '.syncProgress' <<<"$NODE_STATUS" 2>/dev/null || true)"
      [ "''${SYNC_PROGRESS%.*}" -ge "99" ] || exit 1
    '';
  };
}
