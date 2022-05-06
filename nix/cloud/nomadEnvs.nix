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
        job.cardano.group.cardano.task.node.env.CONSUL_KV_TOPOLOGY_PATH = "cardano/vasil-qa/p1";
        job.cardano.group.cardano.task.node.env.VAULT_KV_PATH = "cardano/vasil-qa/p1";
      };
      p2 = merge cardano.nomadJob.default constants.envs.vasil-qa {
        # job.cardano.group.cardano.task.node.env.ENVIRONMENT = "testnet";
        job.cardano.group.cardano.task.node.env.CONSUL_KV_PATH = "cardano/vasil-qa";
        job.cardano.group.cardano.task.node.env.CONSUL_KV_TOPOLOGY_PATH = "cardano/vasil-qa/p2";
        job.cardano.group.cardano.task.node.env.VAULT_KV_PATH = "cardano/vasil-qa/p2";
      };
      p3 = merge cardano.nomadJob.default constants.envs.vasil-qa {
        # job.cardano.group.cardano.task.node.env.ENVIRONMENT = "testnet";
        job.cardano.group.cardano.task.node.env.CONSUL_KV_PATH = "cardano/vasil-qa";
        job.cardano.group.cardano.task.node.env.CONSUL_KV_TOPOLOGY_PATH = "cardano/vasil-qa/p3";
        job.cardano.group.cardano.task.node.env.VAULT_KV_PATH = "cardano/vasil-qa/p3";
      };
    };
  }
