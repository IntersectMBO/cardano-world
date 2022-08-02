{
  inputs,
  cell,
}: let
  inherit (inputs) data-merge;
  inherit (inputs.cells) cardano;
  inherit (cell) constants;

  persistanceMount = "/persist";
in {
  bft-0 = let
    jobname = "cardano-bft-0";
  in
    data-merge.merge (
      cardano.nomadCharts.cardano-node (constants.envs.shelley-qa
        // {
          datacenters = ["eu-central-1"];
          inherit jobname;
        })
    ) {
      job.${jobname}.group.cardano.task.node = {
        # env.ENVIRONMENT = "testnet";
        # env.DEBUG_SLEEP = 6000;
        env.DATA_DIR = persistanceMount + "/bft-0";
        env.CONSUL_KV_PATH = "config/cardano/shelley-qa";
        env.VAULT_KV_PATH = "kv/data/cardano/shelley-qa/bft-0";
        env.LOCAL_ROOTS_SRV_DNS = "_shelley-qa-${jobname}-node._tcp.service.consul";
        env.PUBLIC_ROOTS_SRV_DNS = "_shelley-qa-node._tcp.service.consul";
      };
    };
  bft-1 = let
    jobname = "cardano-bft-1";
  in
    data-merge.merge (
      cardano.nomadCharts.cardano-node (constants.envs.shelley-qa
        // {
          datacenters = ["eu-west-1"];
          inherit jobname;
        })
    ) {
      job.${jobname}.group.cardano.task.node = {
        # env.ENVIRONMENT = "testnet";
        # env.DEBUG_SLEEP = 6000;
        env.DATA_DIR = persistanceMount + "/bft-1";
        env.CONSUL_KV_PATH = "config/cardano/shelley-qa";
        env.VAULT_KV_PATH = "kv/data/cardano/shelley-qa/bft-1";
        env.LOCAL_ROOTS_SRV_DNS = "_shelley-qa-${jobname}-node._tcp.service.consul";
        env.PUBLIC_ROOTS_SRV_DNS = "_shelley-qa-node._tcp.service.consul";
      };
    };
  bft-2 = let
    jobname = "cardano-bft-2";
  in
    data-merge.merge (
      cardano.nomadCharts.cardano-node (constants.envs.shelley-qa
        // {
          datacenters = ["us-east-2"];
          inherit jobname;
        })
    ) {
      job.${jobname}.group.cardano.task.node = {
        # env.ENVIRONMENT = "testnet";
        # env.DEBUG_SLEEP = 6000;
        env.DATA_DIR = persistanceMount + "/bft-2";
        env.CONSUL_KV_PATH = "config/cardano/shelley-qa";
        env.VAULT_KV_PATH = "kv/data/cardano/shelley-qa/bft-2";
        env.LOCAL_ROOTS_SRV_DNS = "_shelley-qa-${jobname}-node._tcp.service.consul";
        env.PUBLIC_ROOTS_SRV_DNS = "_shelley-qa-node._tcp.service.consul";
      };
    };
  sp-1 = let
    jobname = "cardano-sp-1";
  in
    data-merge.merge (
      cardano.nomadCharts.cardano-node (
        constants.envs.shelley-qa
        // {
          datacenters = ["eu-west-1"];
          inherit jobname;
        }
      )
    ) {
      job.${jobname}.group.cardano.task.node = {
        # env.ENVIRONMENT = "testnet";
        # env.DEBUG_SLEEP = 6000;
        env.DATA_DIR = persistanceMount + "/sp-1";
        env.CONSUL_KV_PATH = "config/cardano/shelley-qa";
        env.VAULT_KV_PATH = "kv/data/cardano/shelley-qa/sp-1";
        env.LOCAL_ROOTS_SRV_DNS = "_shelley-qa-${jobname}-node._tcp.service.consul";
        env.PUBLIC_ROOTS_SRV_DNS = "_shelley-qa-node._tcp.service.consul";
      };
    };
  sp-2 = let
    jobname = "cardano-sp-2";
  in
    data-merge.merge (
      cardano.nomadCharts.cardano-node (
        constants.envs.shelley-qa
        // {
          datacenters = ["us-east-2"];
          inherit jobname;
        }
      )
    ) {
      job.${jobname}.group.cardano.task.node = {
        # env.ENVIRONMENT = "testnet";
        # env.DEBUG_SLEEP = 6000;
        env.DATA_DIR = persistanceMount + "/sp-2";
        env.CONSUL_KV_PATH = "config/cardano/shelley-qa";
        env.VAULT_KV_PATH = "kv/data/cardano/shelley-qa/sp-2";
        env.LOCAL_ROOTS_SRV_DNS = "_shelley-qa-${jobname}-node._tcp.service.consul";
        env.PUBLIC_ROOTS_SRV_DNS = "_shelley-qa-node._tcp.service.consul";
      };
    };
  sp-3 = let
    jobname = "cardano-sp-3";
  in
    data-merge.merge (
      cardano.nomadCharts.cardano-node (
        constants.envs.shelley-qa
        // {
          datacenters = ["eu-central-1"];
          inherit jobname;
        }
      )
    ) {
      job.${jobname}.group.cardano.task.node = {
        # env.ENVIRONMENT = "testnet";
        # env.DEBUG_SLEEP = 6000;
        env.DATA_DIR = persistanceMount + "/sp-3";
        env.CONSUL_KV_PATH = "config/cardano/shelley-qa";
        env.VAULT_KV_PATH = "kv/data/cardano/shelley-qa/sp-3";
        env.LOCAL_ROOTS_SRV_DNS = "_shelley-qa-${jobname}-node._tcp.service.consul";
        env.PUBLIC_ROOTS_SRV_DNS = "_shelley-qa-node._tcp.service.consul";
      };
    };
  ogmios-0 = let
    jobname = "ogmios-0";
  in
    data-merge.merge (cardano.nomadCharts.ogmios (
      constants.envs.shelley-qa
      // {
        datacenters = ["eu-central-1"];
        inherit jobname;
      }
    )) {
      job.${jobname}.group.ogmios.task = {
        node = {
          # env.ENVIRONMENT = "testnet";
          # env.DEBUG_SLEEP = 6000;
          env.DATA_DIR = persistanceMount + "/ogmios-0";
          env.CONSUL_KV_PATH = "config/cardano/shelley-qa";
          env.EDGE_NODE = "1";
          env.LOCAL_ROOTS_SRV_DNS = "_shelley-qa-${jobname}-node._tcp.service.consul";
          env.PUBLIC_ROOTS_SRV_DNS = "_shelley-qa-node._tcp.service.consul";
        };
        ogmios = {
          # env.ENVIRONMENT = "testnet";
          # env.DEBUG_SLEEP = 6000;
          env.DATA_DIR = persistanceMount + "/ogmios-0";
          env.CONSUL_KV_PATH = "config/cardano/shelley-qa";
        };
      };
    };
  db-sync-0 = let
    jobname = "db-sync-0";
  in
    data-merge.merge (cardano.nomadCharts.cardano-db-sync (
      constants.envs.shelley-qa
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
          env.DATA_DIR = persistanceMount + "/db-sync-0";
          env.CONSUL_KV_PATH = "config/cardano/shelley-qa";
          env.PUBLIC_ROOTS_SRV_DNS = "_shelley-qa-node._tcp.service.consul";
          env.EDGE_NODE = "1";
        };
        db-sync = {
          # env.ENVIRONMENT = "testnet";
          # env.DEBUG_SLEEP = 6000;
          env.DB_NAME = "shelley_qa_dbsync";
          env.DATA_DIR = persistanceMount + "/db-sync-0";
          env.CONSUL_KV_PATH = "config/cardano/shelley-qa";
          env.VAULT_KV_PATH = "kv/data/db-sync/shelley-qa";
          env.MASTER_REPLICA_SRV_DNS = "_infra-database._master.service.eu-central-1.consul";
        };
      };
    };
}
