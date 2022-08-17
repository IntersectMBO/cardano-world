{
  inputs,
  cell,
}: let
  inherit (inputs) data-merge cells;
  inherit (inputs.nixpkgs) lib;
  inherit (inputs.nixpkgs) system;
  inherit (inputs.bitte-cells) vector _utils;
  inherit (cell) healthChecks constants oci-images;
  # OCI-Image Namer
  ociNamer = oci: l.unsafeDiscardStringContext "${oci.imageName}:${oci.imageTag}";
  l = lib // builtins;
in
  {
    jobname ? "wallet",
    namespace,
    datacenters ? ["eu-central-1" "eu-west-1" "us-east-2"],
    domain,
    extraVector ? {},
    nodeClass,
    scaling,
  } @ args: let
    id = jobname;
    type = "service";
    priority = 50;
    persistanceMount = "/persist";
    vaultPkiPath = "pki/issue/wallet";
    consulRolePath = "consul/creds/wallet";
  in
    with data-merge; {
      job.${id} = {
        inherit namespace datacenters id type priority;
        # ----------
        # Scheduling
        # ----------
        constraint = [
          {
            attribute = "\${node.class}";
            operator = "=";
            value = "${nodeClass}";
          }
          {
            attribute = "\${meta.cardano}";
            operator = "is_set";
          }
          {
            operator = "distinct_hosts";
            value = "true";
          }
        ];
        spread = [{attribute = "\${node.datacenter}";}];
        # ----------
        # Update
        # ----------
        update.health_check = "task_states";
        update.healthy_deadline = "5m0s";
        update.max_parallel = 1;
        update.min_healthy_time = "10s";
        update.progress_deadline = "10m0s";
        update.stagger = "30s";
        # ----------
        # Migrate
        # ----------
        migrate.health_check = "checks";
        migrate.healthy_deadline = "8m20s";
        migrate.max_parallel = 1;
        migrate.min_healthy_time = "10s";
        # ----------
        # Reschedule
        # ----------
        reschedule.delay = "30s";
        reschedule.delay_function = "exponential";
        reschedule.max_delay = "1h0m0s";
        reschedule.unlimited = true;
        # ----------
        # Task Groups
        # ----------
        group.wallet = let
          # work-around: we need to get rid of vector first
          node' = (cell.nomadCharts.cardano-node (args // {jobname = "node";})).job.node.group.cardano;
          group = l.removeAttrs node' ["task"];
          node = group // {task.node = node'.task.node;};
        in
          merge
          # task.vector ...
          (vector.nomadTask.default {
            inherit namespace;
            endpoints = [
              # prometheus metrics for wallet
              "http://127.0.0.1:8082/metrics"
              # prometheus metrics for cardano-node
              "http://127.0.0.1:12798/metrics"
            ];
            extra = extraVector;
          })
          (
            merge node
            {
              count = scaling;
              service = append [
                (import ./srv-wallet.nix {inherit namespace healthChecks;})
              ];
              volume = {
                "persist-wallet-local" = {
                  source = "${namespace}-persist-wallet-local";
                  type = "host";
                };
              };
              network.port.wallet = {to = 8090;};
              task = {
                # ----------
                # Task: Wallet
                # ----------
                wallet = {
                  env.DATA_DIR = persistanceMount;
                  env.SOCKET_PATH = "/alloc/tmp/node.socket";
                  template =
                    _utils.nomadFragments.workload-identity-vault {inherit vaultPkiPath;}
                    ++ _utils.nomadFragments.workload-identity-vault-consul {inherit consulRolePath;};
                  env.WORKLOAD_CACERT = "/secrets/tls/ca.pem";
                  env.WORKLOAD_CLIENT_KEY = "/secrets/tls/key.pem";
                  env.WORKLOAD_CLIENT_CERT = "/secrets/tls/cert.pem";
                  config.image = ociNamer oci-images.cardano-wallet;
                  user = "0:0";
                  driver = "docker";
                  kill_signal = "SIGINT";
                  kill_timeout = "30s";
                  resources = {
                    cpu = 2000;
                    memory = 4096;
                  };
                  volume_mount = {
                    destination = persistanceMount;
                    propagation_mode = "private";
                    volume = "persist-wallet-local";
                  };
                  vault = {
                    change_mode = "noop";
                    env = true;
                    policies = ["wallet"];
                  };
                };
              };
            }
          );
      };
    }
