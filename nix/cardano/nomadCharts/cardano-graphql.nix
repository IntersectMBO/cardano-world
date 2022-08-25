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
    jobname ? "cardano-graphql",
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
    vaultPkiPath = "pki/issue/graphql";
    consulRolePath = "consul/creds/graphql";
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
        group.cardano-graphql = let
          template =
            _utils.nomadFragments.workload-identity-vault {inherit vaultPkiPath;}
            ++ _utils.nomadFragments.workload-identity-vault-consul {inherit consulRolePath;}
            ++ [
              {
                change_mode = "restart";
                data = "{{- with secret \"kv/data/db-sync/${namespace}\" }}{{ .Data.data.pgPass }}{{ end -}}";
                destination = "/secrets/pgpass";
              }
              {
                change_mode = "restart";
                data = "{{- with secret \"kv/data/db-sync/${namespace}\" }}{{ .Data.data.pgUser }}{{ end -}}";
                destination = "/secrets/pguser";
              }
            ];
          secretsEnv = {
            DB_USER_FILE = "/secrets/pguser";
            DB_PASS_FILE = "/secrets/pgpass";
            WORKLOAD_CACERT = "/secrets/tls/ca.pem";
            WORKLOAD_CLIENT_KEY = "/secrets/tls/key.pem";
            WORKLOAD_CLIENT_CERT = "/secrets/tls/cert.pem";
          };

          # work-around: we need to get rid of vector first
          ogmiosFullGroup = (cell.nomadCharts.ogmios (args // {jobname = "ogmios";})).job.ogmios.group.ogmios;
          ogmiosWithoutTask = l.removeAttrs ogmiosFullGroup ["task"];
          ogmios = ogmiosWithoutTask // {
            task.ogmios = ogmiosFullGroup.task.ogmios;
            task.node = ogmiosFullGroup.task.node;
          };
        in
          merge
          # task.vector ...
          (vector.nomadTask.default {
            inherit namespace;
            endpoints = [
              # prometheus metrics for cardano-graphql
              "http://127.0.0.1:8090/metrics"
            ];
            extra = extraVector;
          })
          (
            merge ogmios
              {
                count = scaling;
                # service = append [
                #   (import ./srv-cardano-graphql.nix {inherit namespace healthChecks;})
                # ];
                network.port.http.to = "3100";
                service = append [
                  {
                    name = "cardano-graphql-${namespace}";
                    port = "http";
                    tags = [
                      "ingress"
                      "traefik.enable=true"
                      "traefik.http.routers.cardano-graphql-${namespace}.rule=Host(`graphql.${domain}`) && PathPrefix(`/`)"
                      "traefik.http.routers.cardano-graphql-${namespace}.entrypoints=https"
                      "traefik.http.routers.cardano-graphql-${namespace}.tls.certresolver=acme"
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
                  # Task: Cardano-Graphql
                  # ----------
                  graphql-engine = {
                    env = secretsEnv // {
                      HASURA_PORT = "8090";
                    };
                    inherit template;
                    config.image = ociNamer oci-images.graphql-engine;
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
                      policies = ["graphql"];
                    };
                  };
                  cardano-graphql = {
                    inherit template;
                    env = secretsEnv // {
                      API_PORT = 3100; # Documenting default value
                      HASURA_URI = "http://127.0.0.1:8090";
                      OGMIOS_HOST = "127.0.0.1";
                      OGMIOS_PORT = 1337;
                    };

                    config.image = ociNamer oci-images.cardano-graphql;
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
                      policies = ["graphql"];
                    };
                  };
                };
              }
          );
      };
    }
