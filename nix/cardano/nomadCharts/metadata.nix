{
  inputs,
  cell,
}: let
  inherit (inputs) data-merge cells;
  inherit (inputs.nixpkgs) lib system;
  inherit (inputs.bitte-cells) vector _utils;
  inherit (cell) healthChecks constants oci-images;
  # OCI-Image Namer
  ociNamer = oci: l.unsafeDiscardStringContext "${oci.imageName}:${oci.imageTag}";
  l = lib // builtins;
in
  {
    jobname ? "metadata-server",
    namespace,
    datacenters ? ["eu-central-1" "eu-west-1" "us-east-2"],
    domain,
    extraVector ? {},
    nodeClass,
    ...
  } @ args: let
    id = jobname;
    type = "service";
    priority = 50;
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
            operator = "distinct_hosts";
            value = "true";
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
        update.healthy_deadline = "5m0s";
        update.max_parallel = 1;
        update.min_healthy_time = "10s";
        update.progress_deadline = "10m0s";
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
        reschedule.delay = "10s";
        reschedule.delay_function = "exponential";
        reschedule.max_delay = "5m";
        reschedule.unlimited = true;
        # ----------
        # Task Groups
        # ----------
        group.metadata =
          merge
          # task.vector ...
          (vector.nomadTask.default {
            inherit namespace;
            endpoints = [
              # For when metadata-server metrics metrics scrape endpoint becomes available
            ];
            extra = extraVector;
          })
          {
            count = 1;
            network = {
              mode = "bridge";
              dns = {servers = ["172.17.0.1"];};
              port = {
                server = {};
                varnish = {};
                webhook = {};
              };
            };
            service = [
              (import ./srv-metadata-server.nix {inherit namespace;})
              (import ./srv-metadata-webhook.nix {inherit namespace;})
            ];
            task = {
              # ------------
              # Task: server
              # ------------
              server = {
                env = {
                  PG_DB = "metadata_server";
                  PG_HOST = "master.infra-database.service.consul";
                  PG_TABLE = "metadata";
                  PG_NUM_CONNS = "1";
                  PG_SSL_MODE = "require";
                  PORT = "\${NOMAD_PORT_server}";
                };

                template = [
                  {
                    change_mode = "restart";
                    data = ''
                      PG_USER={{- with secret "kv/data/metadata/${namespace}" }}{{ .Data.data.pgUser }}{{ end }}
                      PG_PASS={{- with secret "kv/data/metadata/${namespace}" }}{{ .Data.data.pgPass }}{{ end }}
                    '';
                    destination = "/secrets/metadata-server-env.sh";
                    env = true;
                  }
                ];

                config.image = ociNamer oci-images.metadata-server;
                driver = "docker";
                kill_signal = "SIGINT";
                kill_timeout = "30s";
                resources = {
                  cpu = 2000;
                  memory = 4 * 1024;
                };
                vault = {
                  change_mode = "noop";
                  env = true;
                  policies = ["metadata"];
                };
              };

              # ----------
              # Task: sync
              # ----------
              sync = {
                env = {
                  PG_DB = "metadata_server";
                  PG_HOST = "master.infra-database.service.consul";
                  PG_TABLE = "metadata";
                  PG_NUM_CONNS = "1";
                  PG_SSL_MODE = "require";
                };

                template = [
                  {
                    change_mode = "restart";
                    data = ''
                      PG_USER={{- with secret "kv/data/metadata/${namespace}" }}{{ .Data.data.pgUser }}{{ end }}
                      PG_PASS={{- with secret "kv/data/metadata/${namespace}" }}{{ .Data.data.pgPass }}{{ end }}
                      GIT_URL={{- with secret "kv/data/metadata/${namespace}" }}{{ .Data.data.gitUrl }}{{ end }}
                      GIT_METADATA_FOLDER={{- with secret "kv/data/metadata/${namespace}" }}{{ .Data.data.gitMetadataFolder }}{{ end }}
                    '';
                    destination = "/secrets/metadata-sync-env.sh";
                    env = true;
                  }
                ];

                config.image = ociNamer oci-images.metadata-sync;
                driver = "docker";
                kill_signal = "SIGINT";
                kill_timeout = "30s";
                resources = {
                  cpu = 2000;
                  memory = 2 * 1024;
                };
                vault = {
                  change_mode = "noop";
                  env = true;
                  policies = ["metadata"];
                };
              };

              # -------------
              # Task: webhook
              # -------------
              webhook = {
                env = {
                  PG_DB = "metadata_server";
                  PG_HOST = "master.infra-database.service.consul";
                  PG_TABLE = "metadata";
                  PG_NUM_CONNS = "1";
                  PG_SSL_MODE = "require";
                  PORT = "\${NOMAD_PORT_webhook}";
                };

                template = [
                  {
                    change_mode = "restart";
                    data = ''
                      PG_USER={{- with secret "kv/data/metadata/${namespace}" }}{{ .Data.data.pgUser }}{{ end }}
                      PG_PASS={{- with secret "kv/data/metadata/${namespace}" }}{{ .Data.data.pgPass }}{{ end }}
                      METADATA_GITHUB_TOKEN={{- with secret "kv/data/metadata/${namespace}" }}{{ .Data.data.metadataGithubToken }}{{ end }}
                      METADATA_WEBHOOK_SECRET={{- with secret "kv/data/metadata/${namespace}" }}{{ .Data.data.metadataWebhookSecret }}{{ end }}
                    '';
                    destination = "/secrets/metadata-webhook-env.sh";
                    env = true;
                  }
                ];

                config.image = ociNamer oci-images.metadata-webhook;
                driver = "docker";
                kill_signal = "SIGINT";
                kill_timeout = "30s";
                resources = {
                  cpu = 2000;
                  memory = 2 * 1024;
                };
                vault = {
                  change_mode = "noop";
                  env = true;
                  policies = ["metadata"];
                };
              };
            };
          };
      };
    }
