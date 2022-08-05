{
  inputs,
  cell,
}: let
  inherit (inputs) data-merge;
  inherit (inputs.cells) cardano;
  inherit (cell) constants;

  persistanceMount = "/persist";
  LEDGER_SLOT = cardano.environments.vasil-dev.usePeersFromLedgerAfterSlot;
in {
  bft-0 = let
    jobname = "cardano-bft-0";
  in
    data-merge.merge (
      cardano.nomadCharts.cardano-node (constants.envs.vasil-dev
        // {
          datacenters = ["eu-central-1"];
          inherit jobname;
        })
    ) {
      job.${jobname}.group.cardano.task.node = {
        # env.ENVIRONMENT = "testnet";
        # env.DEBUG_SLEEP = 6000;
        env = {
          inherit LEDGER_SLOT;
          DATA_DIR = persistanceMount + "/bft-0";
          CONSUL_KV_PATH = "config/cardano/vasil-dev";
          VAULT_KV_PATH = "kv/data/cardano/vasil-dev/bft-0";
          LOCAL_ROOTS_SRV_DNS = "_vasil-dev-${jobname}-node._tcp.service.consul";
          PUBLIC_ROOTS_SRV_DNS = "_vasil-dev-node._tcp.service.consul";
        };
      };
    };
  bft-1 = let
    jobname = "cardano-bft-1";
  in
    data-merge.merge (
      cardano.nomadCharts.cardano-node (constants.envs.vasil-dev
        // {
          datacenters = ["eu-west-1"];
          inherit jobname;
        })
    ) {
      job.${jobname}.group.cardano.task.node = {
        # env.ENVIRONMENT = "testnet";
        # env.DEBUG_SLEEP = 6000;
        env = {
          inherit LEDGER_SLOT;
          DATA_DIR = persistanceMount + "/bft-1";
          CONSUL_KV_PATH = "config/cardano/vasil-dev";
          VAULT_KV_PATH = "kv/data/cardano/vasil-dev/bft-1";
          LOCAL_ROOTS_SRV_DNS = "_vasil-dev-${jobname}-node._tcp.service.consul";
          PUBLIC_ROOTS_SRV_DNS = "_vasil-dev-node._tcp.service.consul";
        };
      };
    };
  bft-2 = let
    jobname = "cardano-bft-2";
  in
    data-merge.merge (
      cardano.nomadCharts.cardano-node (constants.envs.vasil-dev
        // {
          datacenters = ["us-east-2"];
          inherit jobname;
        })
    ) {
      job.${jobname}.group.cardano.task.node = {
        # env.ENVIRONMENT = "testnet";
        # env.DEBUG_SLEEP = 6000;
        env = {
          inherit LEDGER_SLOT;
          DATA_DIR = persistanceMount + "/bft-2";
          CONSUL_KV_PATH = "config/cardano/vasil-dev";
          VAULT_KV_PATH = "kv/data/cardano/vasil-dev/bft-2";
          LOCAL_ROOTS_SRV_DNS = "_vasil-dev-${jobname}-node._tcp.service.consul";
          PUBLIC_ROOTS_SRV_DNS = "_vasil-dev-node._tcp.service.consul";
        };
      };
    };
  sp-1 = let
    jobname = "cardano-sp-1";
  in
    data-merge.merge (
      cardano.nomadCharts.cardano-node (
        constants.envs.vasil-dev
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
          CONSUL_KV_PATH = "config/cardano/vasil-dev";
          VAULT_KV_PATH = "kv/data/cardano/vasil-dev/sp-1";
          LOCAL_ROOTS_SRV_DNS = "_vasil-dev-${jobname}-node._tcp.service.consul";
          PUBLIC_ROOTS_SRV_DNS = "_vasil-dev-node._tcp.service.consul";
        };
      };
    };
  sp-2 = let
    jobname = "cardano-sp-2";
  in
    data-merge.merge (
      cardano.nomadCharts.cardano-node (
        constants.envs.vasil-dev
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
          CONSUL_KV_PATH = "config/cardano/vasil-dev";
          VAULT_KV_PATH = "kv/data/cardano/vasil-dev/sp-2";
          LOCAL_ROOTS_SRV_DNS = "_vasil-dev-${jobname}-node._tcp.service.consul";
          PUBLIC_ROOTS_SRV_DNS = "_vasil-dev-node._tcp.service.consul";
        };
      };
    };
  sp-3 = let
    jobname = "cardano-sp-3";
  in
    data-merge.merge (
      cardano.nomadCharts.cardano-node (
        constants.envs.vasil-dev
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
          CONSUL_KV_PATH = "config/cardano/vasil-dev";
          VAULT_KV_PATH = "kv/data/cardano/vasil-dev/sp-3";
          LOCAL_ROOTS_SRV_DNS = "_vasil-dev-${jobname}-node._tcp.service.consul";
          PUBLIC_ROOTS_SRV_DNS = "_vasil-dev-node._tcp.service.consul";
        };
      };
    };
  ogmios-0 = let
    jobname = "ogmios-0";
  in
    data-merge.merge (cardano.nomadCharts.ogmios (
      constants.envs.vasil-dev
      // {
        datacenters = ["eu-central-1"];
        inherit jobname;
      }
    )) {
      job.${jobname}.group.ogmios.task = {
        node = {
          # env.ENVIRONMENT = "testnet";
          # env.DEBUG_SLEEP = 6000;
          env = {
            DATA_DIR = persistanceMount + "/ogmios-0";
            CONSUL_KV_PATH = "config/cardano/vasil-dev";
            EDGE_NODE = "1";
            LOCAL_ROOTS_SRV_DNS = "_vasil-dev-${jobname}-node._tcp.service.consul";
            PUBLIC_ROOTS_SRV_DNS = "_vasil-dev-node._tcp.service.consul";
          };
        };
        ogmios = {
          # env.ENVIRONMENT = "testnet";
          # env.DEBUG_SLEEP = 6000;
          env = {
            DATA_DIR = persistanceMount + "/ogmios-0";
            CONSUL_KV_PATH = "config/cardano/vasil-dev";
          };
        };
      };
    };
  db-sync-0 = let
    jobname = "db-sync-0";
  in
    data-merge.merge (cardano.nomadCharts.cardano-db-sync (
      constants.envs.vasil-dev
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
            CONSUL_KV_PATH = "config/cardano/vasil-dev";
            PUBLIC_ROOTS_SRV_DNS = "_vasil-dev-node._tcp.service.consul";
            EDGE_NODE = "1";
          };
        };
        db-sync = {
          # env.ENVIRONMENT = "testnet";
          # env.DEBUG_SLEEP = 6000;
          env = {
            DB_NAME = "vasil_dev_dbsync";
            DATA_DIR = persistanceMount + "/db-sync-0";
            CONSUL_KV_PATH = "config/cardano/vasil-dev";
            VAULT_KV_PATH = "kv/data/db-sync/vasil-dev";
            MASTER_REPLICA_SRV_DNS = "_infra-database._master.service.eu-central-1.consul";
          };
        };
      };
    };
  faucet = let
    jobname = "faucet";
  in
    data-merge.merge (cardano.nomadCharts.cardano-faucet (
      constants.envs.vasil-dev
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
            CONSUL_KV_PATH = "config/cardano/vasil-dev";
            PUBLIC_ROOTS_SRV_DNS = "_vasil-dev-node._tcp.service.consul";
            EDGE_NODE = "1";
          };
        };
        cardano-faucet = {
        };
      };
    };
}
