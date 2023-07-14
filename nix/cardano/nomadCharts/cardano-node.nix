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
  ociNamer = oci: builtins.unsafeDiscardStringContext "${oci.imageName}:${oci.imageTag}";

in
  {
    jobname ? "cardano",
    namespace,
    datacenters ? ["eu-central-1" "eu-west-1" "us-east-2"],
    domain,
    extraVector ? {},
    nodeClass,
    scaling,
    # Resource specs used in both resource and RTS flags declaration
    nodeCpuMhz ? 1000,
    nodeMemoryMB ? 8192,
    ...
  }: let
    id = jobname;
    type = "service";
    priority = 50;
    persistanceMount = "/persist";
    vaultPkiPath = "pki/issue/cardano";
    consulRolePath = "consul/creds/cardano";
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
        ];
        spread = [
          {
            attribute = "\${attr.platform.aws.placement.availability-zone}";
            weight = "100";
          }
        ];
        # ----------
        # Update
        # ----------
        # https://www.nomadproject.io/docs/job-specification/update
        update.health_check = "checks";
        update.healthy_deadline = "25m0s";
        update.max_parallel = 1;
        update.min_healthy_time = "10s";
        update.progress_deadline = "30m0s";
        update.stagger = "30s";
        # ----------
        # Migrate
        # ----------
        # https://www.nomadproject.io/docs/job-specification/migrate
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
          # task.vector ...
          (vector.nomadTask.default {
            inherit namespace;
            endpoints = ["http://127.0.0.1:12798/metrics"]; # prometheus metrics for cardano-node
            extra = extraVector;
          })
          {
            count = scaling;
            service = [
              (import ./srv-node.nix {inherit namespace healthChecks;})
              (import ./srv-node-local.nix {inherit namespace jobname;})
            ];
            volume = {
              "persist-cardano-node-local" = {
                source = "${namespace}-persist-cardano-node-local";
                type = "host";
              };
            };
            ephemeral_disk = {
              migrate = true;
              size = 1000;
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
                env.HOST_ADDR = "0.0.0.0";
                env.PORT = "3001";
                env.SOCKET_PATH = "/alloc/tmp/node.socket";
                env.INSTANCE_CPU = "\${attr.cpu.totalcompute}";
                env.INSTANCE_CORES = "\${attr.cpu.numcores}";

                # RTS flags, the string for which is bash evaluated prior to node arg inclusion
                # as some parameters are only determined at runtime.
                # https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/runtime_control.html
                env.RTS_FLAGS = let
                  threads = "$((${toString nodeCpuMhz} / (INSTANCE_CPU / INSTANCE_CORES)))";
                  nValue = ''if [ "${threads}" -eq "0" ]; then echo -n "1"; else echo -n "${threads}"; fi'';
                in "+RTS -N$(${nValue}) -A16m -qg -qb -M${toString (nodeMemoryMB * 0.90)}M -RTS";

                template =
                  _utils.nomadFragments.workload-identity-vault {inherit vaultPkiPath;}
                  ++ _utils.nomadFragments.workload-identity-vault-consul {inherit consulRolePath;};

                env.WORKLOAD_CACERT = "/secrets/tls/ca.pem";
                env.WORKLOAD_CLIENT_KEY = "/secrets/tls/key.pem";
                env.WORKLOAD_CLIENT_CERT = "/secrets/tls/cert.pem";

                config.image = ociNamer oci-images.cardano-node;
                driver = "docker";
                kill_signal = "SIGINT";
                kill_timeout = "30s";
                resources = {
                  cpu = nodeCpuMhz;
                  memory = nodeMemoryMB;
                };
                volume_mount = {
                  destination = persistanceMount;
                  propagation_mode = "private";
                  volume = "persist-cardano-node-local";
                };
                vault = {
                  change_mode = "noop";
                  env = true;
                  policies = ["cardano"];
                };
              };
            };
          };
      };
    }
