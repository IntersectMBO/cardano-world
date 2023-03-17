{
  inputs,
  cell,
}: let
  inherit (inputs.nixpkgs) jq writeText runCommand lib;

  defaultLogConfig = import ./generic-log-config.nix;
  defaultDbSyncLogConfig = import ./db-sync-log-config.nix;
  mkDbSyncConfig = name: nodeConfig:
    (lib.filterAttrs (k: v: v != null) {
      NetworkName = name;
      inherit (nodeConfig) RequiresNetworkMagic;
      NodeConfigFile = "${__toFile "config-${toString name}.json" (__toJSON nodeConfig)}";
    })
    // defaultDbSyncLogConfig;
  mkSubmitApiConfig = name: nodeConfig:
    (lib.filterAttrs (k: v: v != null) {
      GenesisHash = nodeConfig.ByronGenesisHash;
      inherit (nodeConfig) RequiresNetworkMagic;
    })
    // defaultDbSyncLogConfig;

  environments = {
    mainnet = rec {
      useByronWallet = true;
      domain = "cardano-mainnet.iohk.io";
      relays = "relays.cardano-mainnet.iohk.io";
      relaysNew = "relays-new.cardano-mainnet.iohk.io";
      explorerUrl = "https://explorer.cardano.org";
      smashUrl = "https://smash.cardano-mainnet.iohk.io";
      metadataUrl = "https://tokens.cardano.org";
      edgeNodes = [
        "3.125.75.199"
        "18.177.103.105"
        "18.141.0.112"
        "52.14.58.121"
      ];
      edgePort = 3001;
      confKey = "mainnet_full";
      private = false;
      networkConfig = import ./mainnet-config.nix;
      nodeConfig = networkConfig // defaultLogConfig;
      consensusProtocol = networkConfig.Protocol;
      submitApiConfig = mkSubmitApiConfig "mainnet" nodeConfig;
      dbSyncConfig = mkDbSyncConfig "mainnet" nodeConfig;
      usePeersFromLedgerAfterSlot = 84916732;
      auxConfig = import ./aux-config/mainnet-aux.nix inputs;
    };
    # used for daedalus/cardano-wallet for local development
    shelley_qa = rec {
      useByronWallet = false;
      private = true;
      domain = "shelley-qa.dev.cardano.org";
      relaysNew = "relays-new.shelley-qa.dev.cardano.org";
      relaysOld = "shelley-qa-node.world.dev.cardano.org";
      explorerUrl = "https://explorer.shelley-qa.dev.cardano.org";
      smashUrl = "https://smash.shelley-qa.dev.cardano.org";
      metadataUrl = "https://metadata.world.dev.cardano.org";
      networkConfig = import ./shelley_qa-config.nix;
      consensusProtocol = networkConfig.Protocol;
      nodeConfig = defaultLogConfig // networkConfig;
      edgePort = 3001;
      submitApiConfig = mkSubmitApiConfig "shelley_qa" nodeConfig;
      dbSyncConfig = mkDbSyncConfig "shelley_qa" nodeConfig;
      usePeersFromLedgerAfterSlot = 23574838;
    };
    preprod = rec {
      useByronWallet = false;
      private = false;
      domain = "world.dev.cardano.org";
      relaysNew = "preprod-node.world.dev.cardano.org";
      explorerUrl = "https://preprod-explorer.world.dev.cardano.org";
      smashUrl = "https://preprod-smash.world.dev.cardano.org";
      metadataUrl = "https://metadata.world.dev.cardano.org";
      networkConfig = import ./preprod-config.nix;
      consensusProtocol = networkConfig.Protocol;
      nodeConfig = defaultLogConfig // networkConfig;
      edgeNodes = [
        {
          addr = relaysNew;
          port = 30000;
        }
      ];
      submitApiConfig = mkSubmitApiConfig "preprod" nodeConfig;
      dbSyncConfig = mkDbSyncConfig "preprod" nodeConfig;
      usePeersFromLedgerAfterSlot = 4642000;
    };
    preview = rec {
      useByronWallet = false;
      private = false;
      domain = "world.dev.cardano.org";
      relaysNew = "preview-node.world.dev.cardano.org";
      explorerUrl = "https://preview-explorer.world.dev.cardano.org";
      smashUrl = "https://preview-smash.world.dev.cardano.org";
      metadataUrl = "https://metadata.world.dev.cardano.org";
      networkConfig = import ./preview-config.nix;
      consensusProtocol = networkConfig.Protocol;
      nodeConfig = defaultLogConfig // networkConfig;
      edgeNodes = [
        {
          addr = relaysNew;
          port = 30002;
        }
      ];
      submitApiConfig = mkSubmitApiConfig "preview" nodeConfig;
      dbSyncConfig = mkDbSyncConfig "preview" nodeConfig;
      usePeersFromLedgerAfterSlot = 322000;
    };
    private = rec {
      useByronWallet = false;
      private = true;
      domain = "world.dev.cardano.org";
      relaysNew = "private-node.world.dev.cardano.org";
      explorerUrl = "https://private-explorer.world.dev.cardano.org";
      smashUrl = "https://private-smash.world.dev.cardano.org";
      metadataUrl = "https://metadata.world.dev.cardano.org";
      networkConfig = import ./private-config.nix;
      consensusProtocol = networkConfig.Protocol;
      nodeConfig = defaultLogConfig // networkConfig;
      edgeNodes = [
        {
          addr = relaysNew;
          port = 30007;
        }
      ];
      submitApiConfig = mkSubmitApiConfig "private" nodeConfig;
      dbSyncConfig = mkDbSyncConfig "private" nodeConfig;
      usePeersFromLedgerAfterSlot = 32000;
    };
  };
in
  environments
