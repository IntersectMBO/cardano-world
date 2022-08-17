{
  inputs,
  cell,
}: let
  inherit (inputs) data-merge cells nixpkgs;
  inherit (inputs.bitte-cells) vector _utils;
  inherit (cell) healthChecks constants oci-images;
  l = nixpkgs.lib // builtins;
  # OCI-Image Namer
  ociNamer = oci: builtins.unsafeDiscardStringContext "${oci.imageName}:${oci.imageTag}";
in
  {
    jobname ? "db-sync-query",
    namespace,
    datacenters ? ["eu-central-1" "eu-west-1" "us-east-2"],
    domain,
    nodeClass,
    scaling,
  } @ args: let
    id = jobname;
    type = "service";
    priority = 50;
    vaultPkiPath = "pki/issue/db-sync";
    consulRolePath = "consul/creds/db-sync";
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
          # {
          #   attribute = "\${meta.cardano}";
          #   operator = "is_set";
          # }
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
        update.progress_deadline = "60m0s";
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
        group.db-sync-query =
          merge
          # task.vector ...
          (vector.nomadTask.default {
            inherit namespace;
            endpoints = [
              # prometheus metrics for db-sync-query
              "http://127.0.0.1:8090/metrics"
            ];
          })
          (
            {
              count = scaling;
              # service = append [
              #   (import ./srv-db-sync-query.nix {inherit namespace healthChecks;})
              # ];
              network.port.http.to = "8080";
              service = [
                {
                  name = "db-sync-query-${namespace}";
                  port = "http";
                  tags = [
                    "ingress"
                    "traefik.enable=true"
                    "traefik.http.routers.db-sync-query-${namespace}.rule=Host(`db-sync-query.${domain}`)  && PathPrefix(`/`)"
                    "traefik.http.routers.db-sync-query-${namespace}.entrypoints=https"
                    "traefik.http.routers.db-sync-query-${namespace}.tls.certresolver=acme"
                  ];
                  check = [
                    {
                      type = "tcp";
                      port = "http";
                      interval = "10s";
                      timeout = "2s";
                    }
                  ];
                }
              ];

              task = {
                # ----------
                # Task: Db-Sync-Query
                # ----------
                db-sync-query = {

                  template =
                    _utils.nomadFragments.workload-identity-vault {inherit vaultPkiPath;}
                    ++ _utils.nomadFragments.workload-identity-vault-consul {inherit consulRolePath;};

                  env.WORKLOAD_CACERT = "/secrets/tls/ca.pem";
                  env.WORKLOAD_CLIENT_KEY = "/secrets/tls/key.pem";
                  env.WORKLOAD_CLIENT_CERT = "/secrets/tls/cert.pem";
                  config.image = ociNamer oci-images.db-sync-query;
                  config.ports = ["http"];
                  user = "0:0";
                  driver = "docker";
                  kill_signal = "SIGINT";
                  kill_timeout = "30s";
                  resources.cpu = 2000;
                  resources.memory = 4096;
                  vault = {
                    change_mode = "noop";
                    env = true;
                    policies = ["db-sync"];
                  };
                };
              };
            }
          );
      };
    }
