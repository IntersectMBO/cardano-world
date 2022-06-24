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
    runtimeInputs = [nixpkgs.jq nixpkgs.coreutils nixpkgs.curl];
    name = "healthcheck";
    text = ''
      [ -z "''${NOMAD_PORT_wallet:-}" ] && echo "NOMAD_PORT_wallet env var must be set -- aborting" && exit 1

      STATUS="$(curl -sf "localhost:$NOMAD_PORT_wallet/v2/network/information" || :)"
      jq <<< "$STATUS" || :
      jq -e '.sync_progress.status == "ready"' <<< "$STATUS" || exit 1
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
