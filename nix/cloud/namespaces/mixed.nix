{
  inputs,
  cell,
}: let
  inherit (inputs) data-merge;
  inherit (inputs.cells) cardano;
  inherit (cell) constants;

  persistanceMount = "/persist";
  LEDGER_SLOT = cardano.environments.mixed.usePeersFromLedgerAfterSlot;
in {
  sp-1 = let
    jobname = "cardano-sp-1";
  in
    data-merge.merge (
      cardano.nomadCharts.cardano-node (
        constants.envs.mixed
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
          CONSUL_KV_PATH = "config/cardano/mixed";
          VAULT_KV_PATH = "kv/data/cardano/mixed/sp-1";
          LOCAL_ROOTS_SRV_DNS = "_mixed-${jobname}-node._tcp.service.consul";
          PUBLIC_ROOTS_SRV_DNS = "_mixed-node._tcp.service.consul";
        };
      };
    };
  sp-2 = let
    jobname = "cardano-sp-2";
  in
    data-merge.merge (
      cardano.nomadCharts.cardano-node (
        constants.envs.mixed
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
          CONSUL_KV_PATH = "config/cardano/mixed";
          VAULT_KV_PATH = "kv/data/cardano/mixed/sp-2";
          LOCAL_ROOTS_SRV_DNS = "_mixed-${jobname}-node._tcp.service.consul";
          PUBLIC_ROOTS_SRV_DNS = "_mixed-node._tcp.service.consul";
        };
      };
    };
  sp-3 = let
    jobname = "cardano-sp-3";
  in
    data-merge.merge (
      cardano.nomadCharts.cardano-node (
        constants.envs.mixed
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
          CONSUL_KV_PATH = "config/cardano/mixed";
          VAULT_KV_PATH = "kv/data/cardano/mixed/sp-3";
          LOCAL_ROOTS_SRV_DNS = "_mixed-${jobname}-node._tcp.service.consul";
          PUBLIC_ROOTS_SRV_DNS = "_mixed-node._tcp.service.consul";
        };
      };
    };

  /** db-sync-0 = let
    jobname = "db-sync-0";
  in
    data-merge.merge (cardano.nomadCharts.cardano-db-sync (
      constants.envs.mixed
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
            CONSUL_KV_PATH = "config/cardano/mixed";
            PUBLIC_ROOTS_SRV_DNS = "_mixed-node._tcp.service.consul";
            EDGE_NODE = "1";
          };
        };
        db-sync = {
          # env.ENVIRONMENT = "testnet";
          # env.DEBUG_SLEEP = 6000;
          env = {
            DB_NAME = "mixed_dbsync";
            DATA_DIR = persistanceMount + "/db-sync-0";
            CONSUL_KV_PATH = "config/cardano/mixed";
            VAULT_KV_PATH = "kv/data/db-sync/mixed";
            MASTER_REPLICA_SRV_DNS = "_infra-database._master.service.eu-central-1.consul";
          };
        };
      };
    };

  faucet = let
    jobname = "faucet";
  in
    data-merge.merge (cardano.nomadCharts.cardano-faucet (
      constants.envs.mixed
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
            CONSUL_KV_PATH = "config/cardano/mixed";
            PUBLIC_ROOTS_SRV_DNS = "_mixed-node._tcp.service.consul";
            EDGE_NODE = "1";
          };
        };
        cardano-faucet = {
        };
      };
    };*/
}
