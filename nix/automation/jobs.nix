{
  inputs,
  cell,
}: let
  inherit (inputs) nixpkgs;
  inherit (inputs.cells.cardano) packages library nixosProfiles;
  inherit (inputs.bitte-cells._writers.library) writeShellApplication;
  inherit (inputs.nixpkgs.lib.strings) fileContents;
in {
  materialize-node = writeShellApplication {
    name = "materialize-node";
    text = ''
      exec ${packages.cardano-node.passthru.generateMaterialized} ./nix/cardano/packages/materialized
    '';
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
      genesis_dir="$PRJ_ROOT/workbench/custom"
      mkdir -p "$genesis_dir"
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
        --security-param 36 \
        --slot-length 1000 \
        --start-time "$(date --utc +"%Y-%m-%dT%H:%M:%SZ" --date "now +30 min")"
    '';
  };
  gen-custom-kv-config =
    writeShellApplication {
      name = "gen-custom-kv-config";
      runtimeInputs = [nixpkgs.jq nixpkgs.coreutils];
      text = ''
        genesis_dir="$PRJ_ROOT/workbench/custom"
        mkdir -p "$PRJ_ROOT/nix/cloud/kv/consul/cardano"
        mkdir -p "$PRJ_ROOT/nix/cloud/kv/vault/cardano/vasil-qa"
        pushd "$genesis_dir"
          jq -n \
            --arg byron "$(base64 -w 0 < byron-genesis.json)" \
            --arg shelley "$(base64 -w 0 < shelley-genesis.json)" \
            --arg alonzo "$(base64 -w 0 < alonzo-genesis.json)" \
            --argjson config "$(< node-config.json)" \
            '{byronGenesisBlob: $byron, shelleyGenesisBlob: $shelley, alonzoGenesisBlob: $alonzo, nodeConfig: $config}' \
          > config.json
          cp config.json "$PRJ_ROOT/nix/cloud/kv/consul/cardano/vasil-qa.json"
          pushd delegate-keys
            for i in {0..2}; do
              jq -n \
                --argjson cold "$(<shelley."00$i".skey)" \
                --argjson vrf "$(<shelley."00$i".vrf.skey)" \
                --argjson kes "$(<shelley."00$i".kes.skey)" \
                --argjson opcert "$(<shelley."00$i".opcert.json)" \
                --argjson counter "$(<shelley."00$i".counter.json)" \
                --argjson byron_cert "$(<byron."00$i".cert.json)" \
                '{
                  "kes.skey": $kes,
                  "vrf.skey": $vrf,
                  "opcert.json": $opcert,
                  "byron.cert.json": $byron_cert,
                  "cold.skey": $cold,
                  "cold.counter": $counter
                }' > "bft-$i.json"
                cp "bft-$i.json" "$PRJ_ROOT/nix/cloud/kv/vault/cardano/vasil-qa"
            done
          popd
          pushd "$PRJ_ROOT/nix/cloud/kv/vault/cardano/vasil-qa"
            for i in {0..2}; do
              sops -e "bft-$i.json" > "bft-$i.enc.json" && rm "bft-$i.json"
            done
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
