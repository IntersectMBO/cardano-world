{
  inputs,
  cell,
}: let
  inherit (inputs) data-merge;
  inherit (inputs.bitte-cells) vector patroni;
  inherit (inputs.cells) cardano docs;
  inherit (cell) constants;

  mkComponents = args: {
  };

  # Components per environment
  # -----------------------------------------------------------------------
  vasil-qa = mkComponents constants.envs.vasil-qa;
  vasil-dev = mkComponents constants.envs.vasil-dev;
  # TODO: pull this from nomadJob?
  persistanceMount = "/persist";
in
  with data-merge; {
    infra = let
      WALG_S3_PREFIX = "s3://iog-cardano-bitte/backups/infra/walg";
    in {
      documentation = docs.nomadJob.documentation constants.envs.infra;
      database = merge (patroni.nomadJob.default (constants.envs.infra
        // {
          datacenters = ["eu-central-1"];
        })) {
        job.database.group.database.task.patroni.env = {inherit WALG_S3_PREFIX;};
        job.database.group.database.task.backup-walg.env = {inherit WALG_S3_PREFIX;};
      };
    };
    vasil-qa = {
      bft-0 = let
        jobname = "cardano-bft-0";
      in
        merge (
          cardano.nomadJob.cardano-node (constants.envs.vasil-qa
            // {
              datacenters = ["eu-central-1"];
              inherit jobname;
            })
        ) {
          job.${jobname}.group.cardano.task.node = {
            # env.ENVIRONMENT = "testnet";
            # env.DEBUG_SLEEP = 6000;
            env.DATA_DIR = persistanceMount + "/bft-0";
            env.CONSUL_KV_PATH = "config/cardano/vasil-qa";
            env.VAULT_KV_PATH = "kv/data/cardano/vasil-qa/bft-0";
            env.LOCAL_ROOTS_SRV_DNS = "_vasil-qa-${jobname}-node._tcp.service.consul";
            env.PUBLIC_ROOTS_SRV_DNS = "_vasil-qa-node._tcp.service.consul";
          };
        };
      bft-1 = let
        jobname = "cardano-bft-1";
      in
        merge (
          cardano.nomadJob.cardano-node (constants.envs.vasil-qa
            // {
              datacenters = ["eu-west-1"];
              inherit jobname;
            })
        ) {
          job.${jobname}.group.cardano.task.node = {
            # env.ENVIRONMENT = "testnet";
            # env.DEBUG_SLEEP = 6000;
            env.DATA_DIR = persistanceMount + "/bft-1";
            env.CONSUL_KV_PATH = "config/cardano/vasil-qa";
            env.VAULT_KV_PATH = "kv/data/cardano/vasil-qa/bft-1";
            env.LOCAL_ROOTS_SRV_DNS = "_vasil-qa-${jobname}-node._tcp.service.consul";
            env.PUBLIC_ROOTS_SRV_DNS = "_vasil-qa-node._tcp.service.consul";
          };
        };
      bft-2 = let
        jobname = "cardano-bft-2";
      in
        merge (
          cardano.nomadJob.cardano-node (constants.envs.vasil-qa
            // {
              datacenters = ["us-east-2"];
              inherit jobname;
            })
        ) {
          job.${jobname}.group.cardano.task.node = {
            # env.ENVIRONMENT = "testnet";
            # env.DEBUG_SLEEP = 6000;
            env.DATA_DIR = persistanceMount + "/bft-2";
            env.CONSUL_KV_PATH = "config/cardano/vasil-qa";
            env.VAULT_KV_PATH = "kv/data/cardano/vasil-qa/bft-2";
            env.LOCAL_ROOTS_SRV_DNS = "_vasil-qa-${jobname}-node._tcp.service.consul";
            env.PUBLIC_ROOTS_SRV_DNS = "_vasil-qa-node._tcp.service.consul";
          };
        };
      sp-1 = let
        jobname = "cardano-sp-1";
      in
        merge (
          cardano.nomadJob.cardano-node (
            constants.envs.vasil-qa
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
            env.CONSUL_KV_PATH = "config/cardano/vasil-qa";
            env.VAULT_KV_PATH = "kv/data/cardano/vasil-qa/sp-1";
            env.LOCAL_ROOTS_SRV_DNS = "_vasil-qa-${jobname}-node._tcp.service.consul";
            env.PUBLIC_ROOTS_SRV_DNS = "_vasil-qa-node._tcp.service.consul";
          };
        };
      sp-2 = let
        jobname = "cardano-sp-2";
      in
        merge (
          cardano.nomadJob.cardano-node (
            constants.envs.vasil-qa
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
            env.CONSUL_KV_PATH = "config/cardano/vasil-qa";
            env.VAULT_KV_PATH = "kv/data/cardano/vasil-qa/sp-2";
            env.LOCAL_ROOTS_SRV_DNS = "_vasil-qa-${jobname}-node._tcp.service.consul";
            env.PUBLIC_ROOTS_SRV_DNS = "_vasil-qa-node._tcp.service.consul";
          };
        };
      sp-3 = let
        jobname = "cardano-sp-3";
      in
        merge (
          cardano.nomadJob.cardano-node (
            constants.envs.vasil-qa
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
            env.CONSUL_KV_PATH = "config/cardano/vasil-qa";
            env.VAULT_KV_PATH = "kv/data/cardano/vasil-qa/sp-3";
            env.LOCAL_ROOTS_SRV_DNS = "_vasil-qa-${jobname}-node._tcp.service.consul";
            env.PUBLIC_ROOTS_SRV_DNS = "_vasil-qa-node._tcp.service.consul";
          };
        };
      ogmios-0 = let
        jobname = "ogmios-0";
      in
        merge (cardano.nomadJob.ogmios (
          constants.envs.vasil-qa
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
              env.CONSUL_KV_PATH = "config/cardano/vasil-qa";
              env.EDGE_NODE = "1";
              env.LOCAL_ROOTS_SRV_DNS = "_vasil-qa-${jobname}-node._tcp.service.consul";
              env.PUBLIC_ROOTS_SRV_DNS = "_vasil-qa-node._tcp.service.consul";
            };
            ogmios = {
              # env.ENVIRONMENT = "testnet";
              # env.DEBUG_SLEEP = 6000;
              env.DATA_DIR = persistanceMount + "/ogmios-0";
              env.CONSUL_KV_PATH = "config/cardano/vasil-qa";
            };
          };
        };
      db-sync-0 = let
        jobname = "db-sync-0";
      in
        merge (cardano.nomadJob.cardano-db-sync (
          constants.envs.vasil-qa
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
              env.CONSUL_KV_PATH = "config/cardano/vasil-qa";
              env.PUBLIC_ROOTS_SRV_DNS = "_vasil-qa-node._tcp.service.consul";
              env.EDGE_NODE = "1";
            };
            db-sync = {
              # env.ENVIRONMENT = "testnet";
              # env.DEBUG_SLEEP = 6000;
              env.DB_NAME = "vasil_qa_dbsync";
              env.DATA_DIR = persistanceMount + "/db-sync-0";
              env.CONSUL_KV_PATH = "config/cardano/vasil-qa";
              env.VAULT_KV_PATH = "kv/data/db-sync/vasil-qa";
              env.MASTER_REPLICA_SRV_DNS = "_infra-database._master.service.eu-central-1.consul";
            };
          };
        };
    };
    vasil-dev = let
      LEDGER_SLOT = cardano.environments.vasil-dev.usePeersFromLedgerAfterSlot;
    in {
      bft-0 = let
        jobname = "cardano-bft-0";
      in
        merge (
          cardano.nomadJob.cardano-node (constants.envs.vasil-dev
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
        merge (
          cardano.nomadJob.cardano-node (constants.envs.vasil-dev
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
        merge (
          cardano.nomadJob.cardano-node (constants.envs.vasil-dev
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
        merge (
          cardano.nomadJob.cardano-node (
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
        merge (
          cardano.nomadJob.cardano-node (
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
        merge (
          cardano.nomadJob.cardano-node (
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
        merge (cardano.nomadJob.ogmios (
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
        merge (cardano.nomadJob.cardano-db-sync (
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
                DB_NAME = "vasil_qa_dbsync";
                DATA_DIR = persistanceMount + "/db-sync-0";
                CONSUL_KV_PATH = "config/cardano/vasil-dev";
                VAULT_KV_PATH = "kv/data/db-sync/vasil-dev";
                MASTER_REPLICA_SRV_DNS = "_infra-database._master.service.eu-central-1.consul";
              };
            };
          };
        };
    };
  }
