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
      bft-0 = merge (cardano.nomadJob.default constants.envs.vasil-qa // {datacenters = ["eu-central-1"];}) {
        # job.cardano.group.cardano.task.node.env.ENVIRONMENT = "testnet";
        job.cardano.group.cardano.task.node.env.CONSUL_KV_PATH = "cardano/vasil-qa";
        job.cardano.group.cardano.task.node.env.VAULT_KV_PATH = "kv/cardano/vasil-qa/bft-0";
        # job.cardano.group.cardano.task.node.env.LOCAL_ROOTS_SRV_DNS = "_vasil-qa-bft-0-node._tcp.service.consul";
        # job.cardano.group.cardano.task.node.env.PUBLIC_ROOTS_SRV_DNS = "_vasil-qa-node._tcp.service.consul";
        # override with dummy node topology
        job.cardano.group.cardano.task.node.env.NODE_TOPOLOGY = "/local/topology.json";
        job.cardano.group.cardano.task.node.template = append [
          {
            data = builtins.toJSON {Producers = [];};
            destination = "/local/topology.json";
            change_mode = "noop";
          }
        ];
      };
      bft-1 = merge (cardano.nomadJob.default constants.envs.vasil-qa // {datacenters = ["eu-west-1"];}) {
        # job.cardano.group.cardano.task.node.env.ENVIRONMENT = "testnet";
        job.cardano.group.cardano.task.node.env.CONSUL_KV_PATH = "cardano/vasil-qa";
        job.cardano.group.cardano.task.node.env.VAULT_KV_PATH = "kv/cardano/vasil-qa/bft-1";
        job.cardano.group.cardano.task.node.env.LOCAL_ROOTS_SRV_DNS = "_vasil-qa-bft-1-node._tcp.service.consul";
        job.cardano.group.cardano.task.node.env.PUBLIC_ROOTS_SRV_DNS = "_vasil-qa-node._tcp.service.consul";
      };
      bft-2 = merge (cardano.nomadJob.default constants.envs.vasil-qa // {datacenters = ["us-east-2"];}) {
        # job.cardano.group.cardano.task.node.env.ENVIRONMENT = "testnet";
        job.cardano.group.cardano.task.node.env.CONSUL_KV_PATH = "cardano/vasil-qa";
        job.cardano.group.cardano.task.node.env.VAULT_KV_PATH = "kv/cardano/vasil-qa/bft-2";
        job.cardano.group.cardano.task.node.env.LOCAL_ROOTS_SRV_DNS = "_vasil-qa-bft-2-node._tcp.service.consul";
        job.cardano.group.cardano.task.node.env.PUBLIC_ROOTS_SRV_DNS = "_vasil-qa-node._tcp.service.consul";
      };
      sp-1 = merge (cardano.nomadJob.default constants.envs.vasil-qa // {datacenters = ["eu-central-1"];}) {
        # job.cardano.group.cardano.task.node.env.ENVIRONMENT = "testnet";
        job.cardano.group.cardano.task.node.env.CONSUL_KV_PATH = "cardano/vasil-qa";
        job.cardano.group.cardano.task.node.env.VAULT_KV_PATH = "kv/cardano/vasil-qa/sp-1";
        job.cardano.group.cardano.task.node.env.LOCAL_ROOTS_SRV_DNS = "_vasil-qa-sp-1-node._tcp.service.consul";
        job.cardano.group.cardano.task.node.env.PUBLIC_ROOTS_SRV_DNS = "_vasil-qa-node._tcp.service.consul";
      };
    };
  }
