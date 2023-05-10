{
  inputs,
  cell,
}: let
  inherit (inputs) data-merge iohk-nix nixpkgs;
  inherit (inputs.cells) cardano;
  inherit (cell) constants;
  cardanoLib = import "${iohk-nix}/cardano-lib/default.nix" {inherit (nixpkgs) lib writeText runCommand jq;};

  persistanceMount = "/persist";
  LEDGER_SLOT = cardanoLib.environments.private.usePeersFromLedgerAfterSlot;
in {
  sp-1 = let
    jobname = "cardano-sp-1";
  in
    data-merge.merge (
      cardano.nomadCharts.cardano-node (
        constants.envs.private
        // {
          datacenters = ["eu-west-1"];
          inherit jobname;
        }
      )
    ) {
      job.${jobname}.group.cardano.task.node = {
        # env.ENVIRONMENT = "testnet";
        # env.DEBUG_SLEEP = 6000;
        env = {
          inherit LEDGER_SLOT;
          DATA_DIR = persistanceMount + "/sp-1";
          CONSUL_KV_PATH = "config/cardano/private";
          VAULT_KV_PATH = "kv/data/cardano/private/sp-1";
          LOCAL_ROOTS_SRV_DNS = "_private-${jobname}-node._tcp.service.consul";
          PUBLIC_ROOTS_SRV_DNS = "_private-node._tcp.service.consul";
        };
      };
    };
  sp-2 = let
    jobname = "cardano-sp-2";
  in
    data-merge.merge (
      cardano.nomadCharts.cardano-node (
        constants.envs.private
        // {
          datacenters = ["us-east-2"];
          inherit jobname;
        }
      )
    ) {
      job.${jobname}.group.cardano.task.node = {
        # env.ENVIRONMENT = "testnet";
        # env.DEBUG_SLEEP = 6000;
        env = {
          inherit LEDGER_SLOT;
          DATA_DIR = persistanceMount + "/sp-2";
          CONSUL_KV_PATH = "config/cardano/private";
          VAULT_KV_PATH = "kv/data/cardano/private/sp-2";
          LOCAL_ROOTS_SRV_DNS = "_private-${jobname}-node._tcp.service.consul";
          PUBLIC_ROOTS_SRV_DNS = "_private-node._tcp.service.consul";
        };
      };
    };
  sp-3 = let
    jobname = "cardano-sp-3";
  in
    data-merge.merge (
      cardano.nomadCharts.cardano-node (
        constants.envs.private
        // {
          datacenters = ["eu-central-1"];
          inherit jobname;
        }
      )
    ) {
      job.${jobname}.group.cardano.task.node = {
        # env.ENVIRONMENT = "testnet";
        # env.DEBUG_SLEEP = 6000;
        env = {
          inherit LEDGER_SLOT;
          DATA_DIR = persistanceMount + "/sp-3";
          CONSUL_KV_PATH = "config/cardano/private";
          VAULT_KV_PATH = "kv/data/cardano/private/sp-3";
          LOCAL_ROOTS_SRV_DNS = "_private-${jobname}-node._tcp.service.consul";
          PUBLIC_ROOTS_SRV_DNS = "_private-node._tcp.service.consul";
        };
      };
    };

  db-sync-0 = let
    jobname = "db-sync-0";
  in
    data-merge.merge (cardano.nomadCharts.cardano-db-sync (
      constants.envs.private
      // {
        datacenters = ["eu-central-1"];
        inherit jobname;
        scaling = 1;
      }
    )) {
      job.${jobname}.group.db-sync.task = {
        node = {
          # env.ENVIRONMENT = "testnet";
          # env.DEBUG_SLEEP = 6000;
          env = {
            DATA_DIR = persistanceMount + "/db-sync-0";
            CONSUL_KV_PATH = "config/cardano/private";
            PUBLIC_ROOTS_SRV_DNS = "_private-node._tcp.service.consul";
            EDGE_NODE = "1";
          };
        };
        db-sync = {
          # env.ENVIRONMENT = "testnet";
          # env.DEBUG_SLEEP = 6000;
          env = {
            DB_NAME = "private_dbsync";
            ENVIRONMENT = "private";
            DATA_DIR = persistanceMount + "/db-sync-0";
            CONSUL_KV_PATH = "config/cardano/private";
            VAULT_KV_PATH = "kv/data/db-sync/private";
            MASTER_REPLICA_SRV_DNS = "_infra-database._master.service.eu-central-1.consul";
          };
        };
      };
    };

  faucet = let
    jobname = "faucet";
  in
    data-merge.merge (cardano.nomadCharts.cardano-faucet (
      constants.envs.private
      // {
        datacenters = ["eu-central-1"];
        inherit jobname;
        scaling = 1;
      }
    )) {
      job.${jobname}.group.cardano-faucet.task = {
        node = {
          # env.ENVIRONMENT = "testnet";
          # env.DEBUG_SLEEP = 6000;
          env = {
            DATA_DIR = persistanceMount + "/faucet";
            CONSUL_KV_PATH = "config/cardano/private";
            PUBLIC_ROOTS_SRV_DNS = "_private-node._tcp.service.consul";
            EDGE_NODE = "1";
          };
        };
        cardano-faucet = {
        };
      };
    };
}
