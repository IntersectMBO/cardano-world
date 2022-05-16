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
  gen-pool-secrets = writeShellApplication {
    name = "gen-pool-secrets-and-tx";
    runtimeInputs = [nixpkgs.jq nixpkgs.coreutils];
    text = ''
      # Inputs: $PAYMENT_ADDRESS, $NUM_POOLS, $START_INDEX

      # TODO: query utxo to get txin to spend
      TXIN=4d52bb603b3f033b79c6e4325099017df4cecacaab946488fb62ac81e307b467#0
      PAYMENT_ADDRESS=addr_test1vzzm5nunedf27cpv5ma9vk6tt4up4pgw8k8rdpm96u4vm4qt45lrq

      # generate wallet in control of all the funds delegated to the stake pools
      cardano-address recovery-phrase generate > $stake_pool_dir/owner.mnemonic
      # extract stake skey/vkey needed for pool registration and delegation
      for i in {$START_INDEX..$NUM_POOLS}; do cat $stake_pool_dir/owner.mnemonic |cardano-address key from-recovery-phrase Shelley|cardano-address key child 1852H/1815H/"$i"H/2/0|cardano-cli key convert-cardano-address-key --shelley-stake-key --signing-key-file /dev/stdin --out-file /dev/stdout|tee $stake_pool_dir/sp-$i-owner-stake.skey|cardano-cli key verification-key --signing-key-file /dev/stdin --verification-key-file /dev/stdout|cardano-cli key non-extended-key --extended-verification-key-file /dev/stdin --verification-key-file $stake_pool_dir/sp-$i-owner-stake.vkey; done
      # extract reward address vkey
      cat $stake_pool_dir/owner.mnemonic |cardano-address key from-recovery-phrase Shelley|cardano-address key child 1852H/1815H/"0"H/2/0|cardano-cli key convert-cardano-address-key --shelley-stake-key --signing-key-file /dev/stdin --out-file /dev/stdout|cardano-cli key verification-key --signing-key-file /dev/stdin --verification-key-file /dev/stdout|cardano-cli key non-extended-key --extended-verification-key-file /dev/stdin --verification-key-file $stake_pool_dir/sp-0-reward-stake.vkey
      # generate cold/kes/vrf and opcert
      for i in {$START_INDEX..$NUM_POOLS}; do cardano-cli node key-gen --cold-signing-key-file $stake_pool_dir/sp-$i-cold.skey --verification-key-file $stake_pool_dir/sp-$i-cold.vkey --operational-certificate-issue-counter-file $stake_pool_dir/sp-$i-cold.counter; cardano-cli node key-gen-VRF --signing-key-file $stake_pool_dir/sp-$i-vrf.skey --verification-key-file $stake_pool_dir/sp-$i-vrf.vkey; cardano-cli node key-gen-KES --signing-key-file $stake_pool_dir/sp-$i-kes.skey --verification-key-file $stake_pool_dir/sp-$i-kes.vkey; cardano-cli node issue-op-cert --kes-period 0 --kes-verification-key-file $stake_pool_dir/sp-$i-kes.vkey --operational-certificate-issue-counter-file $stake_pool_dir/sp-$i-cold.counter --cold-signing-key-file $stake_pool_dir/sp-$i-cold.skey --out-file $stake_pool_dir/sp-$i.opcert; done
      # generate stake registration and delegation certificates
      for i in {$START_INDEX..$NUM_POOLS}; do cardano-cli stake-address registration-certificate --stake-verification-key-file $stake_pool_dir/sp-$i-owner-stake.vkey --out-file sp-$i-owner-registration.cert; cardano-cli stake-address delegation-certificate --cold-verification-key-file $stake_pool_dir/sp-$i-cold.vkey --stake-verification-key-file $stake_pool_dir/sp-$i-owner-stake.vkey --out-file sp-$i-owner-delegation.cert; done
      # generate stake pool registration certificates
      for i in {$START_INDEX..$NUM_POOLS}; do cardano-cli stake-pool registration-certificate --testnet-magic 9 --cold-verification-key-file $stake_pool_dir/sp-$i-cold.vkey --pool-cost 500000000 --pool-margin 1 --pool-owner-stake-verification-key-file $stake_pool_dir/sp-$i-owner-stake.vkey --pool-pledge 100000000000000 --single-host-pool-relay vasil-qa-node.world.dev.cardano.org --pool-relay-port 30000 --pool-reward-account-verification-key-file $stake_pool_dir/sp-0-reward-stake.vkey --vrf-verification-key-file $stake_pool_dir/sp-$i-vrf.vkey --out-file sp-$i-registration.cert; done
      # generate transaction
      cardano-cli transaction build --tx-in "$TXIN" --change-address "$PAYMENT_ADDRESS" --witness-override 7 $(for i in {$START_INDEX..$NUM_POOLS}; do echo " --certificate-file sp-$i-owner-registration.cert --certificate-file sp-$i-registration.cert --certificate-file sp-$i-owner-delegation.cert"; done) --testnet-magic 9 --out-file tx-pool-reg.txbody
      # sign with keys
      cardano-cli transaction sign --tx-body-file tx-pool-reg.txbody --out-file tx-pool-reg.txsigned --signing-key-file ~/keys-vasil-qa/utxo-keys/rich-utxo.skey $(for i in {$START_INDEX..3}; do echo " --signing-key-file /home/sam/keys-vasil-qa/stake-pools/sp-$i-cold.skey --signing-key-file /home/sam/keys-vasil-qa/stake-pools/sp-$i-owner-stake.skey"; done)
    '';
  };
}
