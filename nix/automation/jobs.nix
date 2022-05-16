{
  inputs,
  cell,
}: let
  inherit (inputs) nixpkgs;
  inherit (inputs.cells.cardano) packages library nixosProfiles;
  inherit (inputs.bitte-cells._writers.library) writeShellApplication;
  inherit (inputs.nixpkgs.lib.strings) fileContents;
in {
  materialize-node = {
    command = ''
      cp ${packages.cardano-node.passthru.generateMaterialized} ./cells/cardano/packages/materialized
    '';
    # dependencies = [packages.cardano-node.passthru.generateMaterialized];
  };
  # run-local-node = let
  #   envName = "testnet";
  #   config =
  #     library.evalNodeConfig envName
  #     nixosProfiles.run-node-testnet;
  #   cmd = writeShellApplication {
  #     name = "run-node";
  #     text =
  #       (fileContents ./../entrypoints/node-entrypoint.sh)
  #       + "\n"
  #       + config.script;
  #     env = {
  #       inherit (config) stateDir socketPath;
  #       inherit envName;
  #     };
  #     runtimeInputs = [
  #       packages.cardano-node
  #       packages.cardano-cli
  #       # TODO: take from somewhere else than aws, e.g. an iohk hydra published path or similar
  #       nixpkgs.awscli2
  #       nixpkgs.gnutar
  #       nixpkgs.gzip
  #     ];
  #   };
  # in {
  #   command = "run-node";
  #   dependencies = [cmd];
  # };
  # push-snapshot-node = let
  #   envName = "testnet";
  #   config =
  #     library.evalNodeConfig envName
  #     nixosProfiles.run-node-testnet;
  #   cmd = writeShellApplication {
  #     name = "push-snapshot";
  #     text = fileContents ./push-node-snapshot.sh;
  #     env = {
  #       inherit (config) stateDir;
  #       inherit envName;
  #     };
  #     runtimeInputs = [nixpkgs.awscli2 nixpkgs.gnutar nixpkgs.gzip nixpkgs.coreutils];
  #   };
  # in {
  #   command = "push-snapshot";
  #   dependencies = [cmd];
  # };
  gen-custom-node-config = writeShellApplication {
    name = "gen-custom-node-config";
    runtimeInputs = [packages.cardano-cli nixpkgs.coreutils];
    text = ''
      genesis_dir="workbench/custom"
      cardano-cli genesis create-cardano \
        --genesis-dir "$genesis_dir" \
        --gen-genesis-keys 3 \
        --supply 30000000000000000 \
        --testnet-magic 9 \
        --slot-coefficient 0.05 \
        --byron-template "$PRJ_ROOT"/nix/cardano/environments/testnet-template/byron.json \
        --shelley-template "$PRJ_ROOT"/nix/cardano/environments/testnet-template/shelley.json \
        --alonzo-template "$PRJ_ROOT"/nix/cardano/environments/testnet/alonzo-genesis.json \
        --node-config-template "$PRJ_ROOT"/nix/cardano/environments/testnet-template/config.json \
        --security-param 108 \
        --slot-length 1000 \
        --start-time "$(date --utc +"%Y-%m-%dT%H:%M:%SZ" --date "now +30 min")"
    '';
  };
  gen-custom-kv-config =
    writeShellApplication {
      name = "gen-custom-kv-config";
      runtimeInputs = [nixpkgs.jq nixpkgs.coreutils];
      text = ''
        genesis_dir="/workbench/custom"
        pushd "$genesis_dir"
          jq -n \
            --arg byron "$(base64 -w 0 < byron-genesis.json)" \
            --arg shelley "$(base64 -w 0 < shelley-genesis.json)" \
            --arg alonzo "$(base64 -w 0 < alonzo-genesis.json)" \
            --argjson config "$(< node-config.json)" \
            '{byronGenesisBlob: $byron, shelleyGenesisBlob: $shelley, alonzoGenesisBlob: $alonzo, nodeConfig: $config}' \
          > config.json
          cp config.json "$PRJ_ROOT"/nix/cloud/kv/consul/cardano/custom.json
          jq -n 'reduce inputs as $s (.; (.[input_filename|sub(".000"; "")]) += $s)' delegate-keys/*.000.{kes.skey,vrf.skey,opcert.json,cert.json,skey} > bft-0.json
          jq -n 'reduce inputs as $s (.; (.[input_filename|sub(".001"; "")]) += $s)' delegate-keys/*.001.{kes.skey,vrf.skey,opcert.json,cert.json,skey} > bft-1.json
          jq -n 'reduce inputs as $s (.; (.[input_filename|sub(".002"; "")]) += $s)' delegate-keys/*.002.{kes.skey,vrf.skey,opcert.json,cert.json,skey} > bft-2.json
          cp bft-* "$PRJ_ROOT"/nix/cloud/kv/vault/cardano/custom
          pushd "$PRJ_ROOT"/nix/cloud/kv/vault/cardano/custom
            sops -e bft-0.json > bft-0.enc.json && rm bft-0.json
            sops -e bft-1.json > bft-1.enc.json && rm bft-1.json
            sops -e bft-2.json > bft-2.enc.json && rm bft-2.json
          popd
        popd
      '';
    }
    // {after = ["gen-custom-node-config"];};
  push-custom-kv-config =
    writeShellApplication {
      name = "push-custom-kv-config";
      runtimeInputs = [nixpkgs.jq nixpkgs.coreutils];
      text = ''
        nix run .#clusters.cardano.tf.hydrate-app.plan
        nix run .#clusters.cardano.tf.hydrate-app.apply
      '';
    }
    // {after = ["gen-custom-kv-config"];};
}
