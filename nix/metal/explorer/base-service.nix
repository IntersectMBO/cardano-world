privateIP: {self, pkgs, config, lib, ...}:
let
  inherit (self.inputs) nixpkgs iohk-nix;
  inherit (lib) mkDefault mkIf mkOption types;
  cardanoLib = import "${iohk-nix}/cardano-lib/default.nix" {inherit (nixpkgs) lib writeText runCommand jq;};

  cfg = config.services.cardano-node;
  nodePort = 3001;

  cardanoNodePrometheusExporterPort = 12798;

  environments = cardanoLib.environments;
  environmentConfig = environments.${cfg.environmentName};

  environmentVariables = let
    genesisFile = environmentConfig.nodeConfig.ShelleyGenesisFile;
    genesis =  builtins.fromJSON (builtins.readFile genesisFile);
  in rec {
    ENVIRONMENT = cfg.environmentName;
    RELAYS = environmentConfig.relaysNew;
    DOMAIN = environmentConfig.domain;

    GENESIS_PATH = toString genesisFile;

    # Network parameters.
    NETWORK_MAGIC = toString genesis.networkMagic;
    EPOCH_LENGTH = toString genesis.epochLength;
    SLOT_LENGTH = toString genesis.slotLength;
    K = toString genesis.securityParam;
    F = toString genesis.activeSlotsCoeff;
    MAX_SUPPLY = toString genesis.maxLovelaceSupply;
  } // (lib.optionalAttrs (builtins.pathExists genesisFile) {
    SYSTEM_START = genesis.systemStart;
    # End: Network parameters.
  }) // (lib.optionalAttrs (environmentConfig.nodeConfig ? ByronGenesisFile) {
    BYRON_GENESIS_PATH = toString environmentConfig.nodeConfig.ByronGenesisFile;
  });

  nodePkgs = self.inputs.explorer-cardano-node.legacyPackages.${cfg.system}.cardanoNodePackages;
in
{
  imports = [(self.inputs.explorer-cardano-node + "/nix/nixos")];

  options = {
    services.cardano-node = {
      environmentName = mkOption {
        # Required to build the correct environment
        type = types.str;
      };

      totalCpuCores = mkOption {
        type = types.int;
        default = 2 * cfg.instances;
      };

      totalMaxHeapSizeMbytes = mkOption {
        type = types.float;
        # Min 14 GB RAM required for mainnet resync
        default = 14 * 1024;
      };

      system = mkOption {
        type = types.str;
        default = "x86_64-linux";
      };
    };
  };

  config = {
    environment.systemPackages = with nodePkgs; [ cardano-cli ];
    environment.variables = environmentVariables // {CARDANO_NODE_SOCKET_PATH = cfg.socketPath;};

    networking.firewall.allowedTCPPorts = [nodePort];

    services.cardano-node = {
      enable = true;
      systemdSocketActivation = true;

      # https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/runtime_control.html
      rtsArgs = [ "-N${toString (cfg.totalCpuCores / cfg.instances)}" "-A16m" "-qg" "-qb" "-M${toString (cfg.totalMaxHeapSizeMbytes / cfg.instances)}M" ];

      environment = cfg.environmentName;
      environments.${cfg.environmentName} = environmentConfig;

      cardanoNodePackages = lib.mkDefault nodePkgs;

      hostAddr = privateIP;

      ipv6HostAddr = mkIf (cfg.instances > 1) "::1";
      port = nodePort;
      nodeConfig = environmentConfig.nodeConfig;

      extraNodeConfig = {
        hasPrometheus = [ privateIP cardanoNodePrometheusExporterPort ];

        # The maximum number of used peers when fetching newly forged blocks:
        MaxConcurrencyDeadline = 4;

        # Use Journald output:
        setupScribes = [{
          scKind = "JournalSK";
          scName = "cardano";
          scFormat = "ScText";
        }];

        defaultScribes = [
          [
            "JournalSK"
            "cardano"
          ]
        ];

        # TraceMempool makes cpu usage x3, disabling by default:
        TraceMempool = false;
      };

      extraServiceConfig = _: {
        serviceConfig = {
          # Allow time to uncompress when restoring db
          TimeoutStartSec = "1h";
          MemoryMax = "${toString (1.15 * cfg.totalMaxHeapSizeMbytes / cfg.instances)}M";
          LimitNOFILE = "65535";
        };
      };
    };

    systemd.services.cardano-node = {
      path = with pkgs; [ gnutar gzip ];

      preStart = ''
        cd $STATE_DIRECTORY
        if [ -f db-restore.tar.gz ]; then
          rm -rf db-${cfg.environmentName}*
          tar xzf db-restore.tar.gz
          rm db-restore.tar.gz
        fi
      '';
    };

    users.users.cardano-node.isSystemUser = true;
    services.dnsmasq.enable = true;
  };
}
