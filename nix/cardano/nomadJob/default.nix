{
  inputs,
  cell,
}: let
  inherit (inputs) data-merge cells;
  inherit (inputs.nixpkgs) lib;
  inherit (inputs.nixpkgs) system;
  inherit (cell) healthChecks constants oci-images;
  # OCI-Image Namer
  ociNamer = oci: "${oci.imageName}:${oci.imageTag}";
in
  with data-merge; {
    default = {
      namespace,
      datacenters ? ["eu-central-1" "eu-west-1" "us-east-2"],
      domain,
      nodeClass,
      scaling,
    }: let
      id = "cardano";
      type = "service";
      priority = 50;
      persistanceMount = "/persist";
    in {
      job.cardano = {
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
        group.cardano =
          merge
          (cells.vector.nomadTask.default {
            endpoints = ["http://127.0.0.1:12798/metrics"]; # prometheus metrics for cardano-node
          })
          {
            count = scaling;
            service = [
              (import ./srv-node.nix {inherit namespace healthChecks;})
            ];
            volume = {
              "persist-cardano-node-local" = {
                source = "${namespace}-persist-cardano-node-local";
                type = "host";
              };
            };
            ephemeral_disk = {
              migrate = true;
              size = 80000;
              sticky = true;
            };
            network = {
              dns = {servers = ["172.17.0.1"];};
              mode = "bridge";
              port.node = {to = 3001;};
            };
            task = {
              # ----------
              # Task: Node
              # ----------
              node = {
                env.DATA_DIR = persistanceMount;
                env.CONSUL_HTTP_ADDR = "testnet"; # not self-contained; only defaults
                env.CONSUL_CACERT = "testnet"; # not self-contained; only defaults
                env.CONSUL_CLIENT_CERT = "testnet"; # not self-contained; only defaults
                env.CONSUL_CLIENT_KEY = "testnet"; # not self-contained; only defaults
                config.image = ociNamer oci-images.cardano-node;
                driver = "docker";
                kill_signal = "SIGINT";
                kill_timeout = "30s";
                resources = {
                  cpu = 5000;
                  memory = 8192;
                };
                volume_mount = {
                  destination = persistanceMount;
                  propagation_mode = "private";
                  volume = "persist-cardano-node-local";
                };
              };
            };
          };
      };
    };
  }
