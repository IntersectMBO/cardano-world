{
  inputs,
  cell,
}: let
  inherit (inputs) data-merge;
  inherit (inputs.cells) cardano;
  inherit (cell) constants;

  persistanceMount = "/persist";
  LEDGER_SLOT = cardano.environments.mainnet.usePeersFromLedgerAfterSlot;
in {
  db-sync-0 = let
    jobname = "db-sync-0";
  in
    data-merge.merge (cardano.nomadCharts.cardano-db-sync (
      constants.envs.mainnet
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
            ENVIRONMENT="mainnet";
            DATA_DIR = persistanceMount + "/db-sync-0";
            CONSUL_KV_PATH = "config/cardano/mainnet";
            PUBLIC_ROOTS_SRV_DNS = "_mainnet-node._tcp.service.consul";
            EDGE_NODE = "1";
          };
        };
        db-sync = {
          # env.ENVIRONMENT = "testnet";
          # env.DEBUG_SLEEP = 6000;
          env = {
            DB_NAME = "mainnet_dbsync";
            ENVIRONMENT = "mainnet";
            DATA_DIR = persistanceMount + "/db-sync-0";
            CONSUL_KV_PATH = "config/cardano/mainnet";
            VAULT_KV_PATH = "kv/data/db-sync/mainnet";
            MASTER_REPLICA_SRV_DNS = "_infra-database._master.service.eu-central-1.consul";
          };
        };
      };
    };

  db-sync-query = let
    jobname = "db-sync-query";
  in
    data-merge.merge (cardano.nomadCharts.db-sync-query (
      constants.envs.mainnet
      // {
        datacenters = ["eu-central-1"];
        inherit jobname;
        scaling = 1;
      }
    )) {
      job.${jobname}.group.db-sync-query.task = {
        db-sync-query = {
          # env.ENVIRONMENT = "testnet";
          # env.DEBUG_SLEEP = 6000;
          env = {
            DB_NAME = "mainnet_dbsync";
            CONSUL_KV_PATH = "config/cardano/mainnet";
            VAULT_KV_PATH = "kv/data/db-sync/mainnet";
            MASTER_REPLICA_SRV_DNS = "_infra-database._master.service.eu-central-1.consul";
          };
        };
      };
    };

}
