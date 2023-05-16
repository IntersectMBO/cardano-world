{self, pkgs, config, lib, ...}:
let
  inherit (lib) mkOption types;

  cfg = config.services.cardano-db-sync;
  nodeCfg = config.services.cardano-node;

  cardanoNodeConfigPath = builtins.toFile "cardano-node-config.json" (builtins.toJSON nodeCfg.nodeConfig);

  environments = pkgs.cardanoLib.environments;
  environmentConfig = environments.${cfg.environmentName};

  dbSyncPkgs = self.inputs.explorer-cardano-db-sync.legacyPackages.${cfg.system};
  inherit (dbSyncPkgs) cardano-db-sync cardano-db-tool;
in {

  imports = [
    (self.inputs.explorer-cardano-db-sync + "/nix/nixos")
    ./cardano-postgres.nix
  ];

  options = {
    services.cardano-db-sync = {
      additionalDbUsers = mkOption {
        type = types.listOf types.str;
        default = [];
      };

      environmentName = mkOption {
        # Required to build the correct environment
        type = types.str;
      };

      system = mkOption {
        type = types.str;
        default = "x86_64-linux";
      };
    };
  };

  config = {
    environment.systemPackages = with pkgs; [
      bat fd lsof netcat ncdu ripgrep tree vim dnsutils
      cardano-db-tool
    ];

    services.cardano-postgres.enable = true;
    services.postgresql = {
      ensureDatabases = ["cexplorer"];
      initialScript = builtins.toFile "enable-pgcrypto.sql" ''
        \connect template1
        CREATE EXTENSION IF NOT EXISTS pgcrypto SCHEMA pg_catalog;
      '';
      ensureUsers = [
        {
          name = "cexplorer";
          ensurePermissions = {
            "DATABASE cexplorer" = "ALL PRIVILEGES";
            "ALL TABLES IN SCHEMA information_schema" = "SELECT";
            "ALL TABLES IN SCHEMA pg_catalog" = "SELECT";
          };
        }
      ];
      identMap = ''
        explorer-users postgres postgres
      ${lib.concatMapStrings (user: ''
        explorer-users ${user} cexplorer
      '') (["root" "cardano-db-sync" ] ++ cfg.additionalDbUsers)}'';
      authentication = ''
        local all all ident map=explorer-users
      '';
    };

    services.cardano-db-sync = {
      inherit dbSyncPkgs;
      enable = true;
      package = cardano-db-sync;
      cluster = cfg.environmentName;
      environment = environmentConfig;
      socketPath = nodeCfg.socketPath;
      explorerConfig = environmentConfig.dbSyncConfig;
      logConfig = {};
      postgres.database = "cexplorer";
    };

    systemd.services.cardano-db-sync.serviceConfig = {
      # FIXME: https://github.com/input-output-hk/cardano-db-sync/issues/102
      Restart = "always";
      RestartSec = "30s";
    };
  };
}
