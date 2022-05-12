{
  inputs,
  cell,
}: let
  inherit (inputs) data-merge;
  inherit (inputs.bitte-cells) vector;
  inherit (inputs.cells) cardano;
  inherit (cell) constants;

  mkComponents = args: {
  };

  # Components per environment
  # -----------------------------------------------------------------------
  vasil-qa = mkComponents constants.envs.vasil-qa;
in
  with data-merge; {
    vasil-qa = {
      bft-0 =
        merge (
          cardano.nomadJob.default (constants.envs.vasil-qa
            // {
              datacenters = ["eu-central-1"];
              jobname = "cardano-bft-0";
            })
        ) {
          # job."cardano-bft-0".group.cardano.task.node.env.ENVIRONMENT = "testnet";
          # job."cardano-bft-0".group.cardano.task.node.env.DEBUG_SLEEP = 6000;
          job."cardano-bft-0".group.cardano.task.node.env.CONSUL_KV_PATH = "config/cardano/vasil-qa";
          job."cardano-bft-0".group.cardano.task.node.env.VAULT_KV_PATH = "kv/data/cardano/vasil-qa/bft-0";
          # job."cardano-bft-0".group.cardano.task.node.env.LOCAL_ROOTS_SRV_DNS = "_vasil-qa-bft-0-node._tcp.service.consul";
          # job."cardano-bft-0".group.cardano.task.node.env.PUBLIC_ROOTS_SRV_DNS = "_vasil-qa-node._tcp.service.consul";
          # override with dummy node topology
          job."cardano-bft-0".group.cardano.task.node.env.NODE_TOPOLOGY = "/local/topology.json";
          job."cardano-bft-0".group.cardano.task.node.template = append [
            {
              data = builtins.toJSON {Producers = [];};
              destination = "/local/topology.json";
              change_mode = "noop";
            }
          ];
        };
      bft-1 =
        merge (
          cardano.nomadJob.default (constants.envs.vasil-qa
            // {
              datacenters = ["eu-west-1"];
              jobname = "cardano-bft-1";
            })
        ) {
          # job."cardano-bft-1".group.cardano.task.node.env.ENVIRONMENT = "testnet";
          # job."cardano-bft-1".group.cardano.task.node.env.DEBUG_SLEEP = 6000;
          job."cardano-bft-1".group.cardano.task.node.env.CONSUL_KV_PATH = "config/cardano/vasil-qa";
          job."cardano-bft-1".group.cardano.task.node.env.VAULT_KV_PATH = "kv/data/cardano/vasil-qa/bft-1";
          # job."cardano-bft-1".group.cardano.task.node.env.LOCAL_ROOTS_SRV_DNS = "_vasil-qa-bft-1-node._tcp.service.consul";
          # job."cardano-bft-1".group.cardano.task.node.env.PUBLIC_ROOTS_SRV_DNS = "_vasil-qa-node._tcp.service.consul";
          # override with dummy node topology
          job."cardano-bft-1".group.cardano.task.node.env.NODE_TOPOLOGY = "/local/topology.json";
          job."cardano-bft-1".group.cardano.task.node.template = append [
            {
              data = builtins.toJSON {Producers = [];};
              destination = "/local/topology.json";
              change_mode = "noop";
            }
          ];
        };
      bft-2 =
        merge (
          cardano.nomadJob.default (constants.envs.vasil-qa
            // {
              datacenters = ["us-east-2"];
              jobname = "cardano-bft-2";
            })
        ) {
          # job."cardano-bft-2".group.cardano.task.node.env.ENVIRONMENT = "testnet";
          # job."cardano-bft-2".group.cardano.task.node.env.DEBUG_SLEEP = 6000;
          job."cardano-bft-2".group.cardano.task.node.env.CONSUL_KV_PATH = "config/cardano/vasil-qa";
          job."cardano-bft-2".group.cardano.task.node.env.VAULT_KV_PATH = "kv/data/cardano/vasil-qa/bft-2";
          # job."cardano-bft-2".group.cardano.task.node.env.LOCAL_ROOTS_SRV_DNS = "_vasil-qa-bft-2-node._tcp.service.consul";
          # job."cardano-bft-2".group.cardano.task.node.env.PUBLIC_ROOTS_SRV_DNS = "_vasil-qa-node._tcp.service.consul";
          # override with dummy node topology
          job."cardano-bft-2".group.cardano.task.node.env.NODE_TOPOLOGY = "/local/topology.json";
          job."cardano-bft-2".group.cardano.task.node.template = append [
            {
              data = builtins.toJSON {Producers = [];};
              destination = "/local/topology.json";
              change_mode = "noop";
            }
          ];
        };
      sp-0 =
        merge (
          cardano.nomadJob.default (
            constants.envs.vasil-qa
            // {
              datacenters = ["eu-central-1"];
              jobname = "cardano-sp-0";
            }
          )
        ) {
          # job.${jobname}.group.cardano.task.node.env.ENVIRONMENT = "testnet";
          job.${jobname}.group.cardano.task.node.env.CONSUL_KV_PATH = "config/cardano/vasil-qa";
          job.${jobname}.group.cardano.task.node.env.VAULT_KV_PATH = "kv/data/cardano/vasil-qa/sp-1";
          job.${jobname}.group.cardano.task.node.env.LOCAL_ROOTS_SRV_DNS = "_vasil-qa-sp-1-node._tcp.service.consul";
          job.${jobname}.group.cardano.task.node.env.PUBLIC_ROOTS_SRV_DNS = "_vasil-qa-node._tcp.service.consul";
        };
    };
  }
