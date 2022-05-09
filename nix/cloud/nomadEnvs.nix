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
      p1 = merge (cardano.nomadJob.default constants.envs.vasil-qa) {
        # job.cardano.group.cardano.task.node.env.ENVIRONMENT = "testnet";
        job.cardano.group.cardano.task.node.env.CONSUL_KV_PATH = "cardano/vasil-qa";
        job.cardano.group.cardano.task.node.env.VAULT_KV_PATH = "cardano/vasil-qa/p1";
        # job.cardano.group.cardano.task.node.env.LOCAL_ROOTS_SRV_DNS = "_vasil-qa-p1-node._tcp.service.consul";
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
      p2 = merge cardano.nomadJob.default constants.envs.vasil-qa {
        # job.cardano.group.cardano.task.node.env.ENVIRONMENT = "testnet";
        job.cardano.group.cardano.task.node.env.CONSUL_KV_PATH = "cardano/vasil-qa";
        job.cardano.group.cardano.task.node.env.VAULT_KV_PATH = "cardano/vasil-qa/p2";
        job.cardano.group.cardano.task.node.env.LOCAL_ROOTS_SRV_DNS = "_vasil-qa-p2-node._tcp.service.consul";
        job.cardano.group.cardano.task.node.env.PUBLIC_ROOTS_SRV_DNS = "_vasil-qa-node._tcp.service.consul";
      };
      p3 = merge cardano.nomadJob.default (constants.envs.vasil-qa // {pool = "p3";}) {
        # job.cardano.group.cardano.task.node.env.ENVIRONMENT = "testnet";
        job.cardano.group.cardano.task.node.env.CONSUL_KV_PATH = "cardano/vasil-qa";
        job.cardano.group.cardano.task.node.env.VAULT_KV_PATH = "cardano/vasil-qa/p3";
        job.cardano.group.cardano.task.node.env.LOCAL_ROOTS_SRV_DNS = "_vasil-qa-p3-node._tcp.service.consul";
        job.cardano.group.cardano.task.node.env.PUBLIC_ROOTS_SRV_DNS = "_vasil-qa-node._tcp.service.consul";
      };
    };
  }
