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
    varnishTtlSec ? 900,
    varnishMemoryMb ? 384, # Actual memory available for caching is about 1/3 this value
    varnishMaxPostSizeBodyKb ? 64, # The maximum POST size allowed for a metadata/query body payload
    varnishMaxPostSizeCachableKb ? 100, # The maximum cacheable POST size before varnish will disconnect and cause traefik to 502
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
              # For when metadata-server metrics scrape endpoint becomes available
            ];
            extra = extraVector;
          })
          {
            count = 1;
            network = {
              mode = "bridge";
              dns = {servers = ["172.17.0.1"];};
              port = {
                server1 = {};
                server2 = {};
                varnish = {};
                webhook = {};
              };
            };
            service = [
              (import ./srv-metadata-server.nix {inherit namespace; instance = 1;})
              (import ./srv-metadata-server.nix {inherit namespace; instance = 2;})
              (import ./srv-metadata-varnish.nix {inherit namespace;})
              (import ./srv-metadata-webhook.nix {inherit namespace;})
            ];
            task = let
              mkServerTask = instance: let
                memoryMb = 4 * 1024;
              in {
                env = {
                  PG_DB = "metadata_server";
                  PG_HOST = "master.infra-database.service.consul";
                  PG_TABLE = "metadata";
                  PG_NUM_CONNS = "1";
                  PG_SSL_MODE = "require";
                  PORT = "\${NOMAD_PORT_server${toString instance}}";
                  MASTER_REPLICA_SRV_DNS = "_infra-database._master.service.eu-central-1.consul";

                  # Server appears to have a memory leak hasn't been resolved, so rather than enforce
                  # GHC heap compliance with the var below, allow OOM to kill the server task
                  # when server overruns it's allocated RAM.  Otherwise server task will just fail to
                  # function properly while other parts of the job stay alive.  Uncomment below once
                  # the memleak is fixed.
                  #
                  # Leave at least a few MB available for overhead and debug entrypoint activity
                  # GHCRTS = "-M${toString (memoryMb - 128)}M";
                };

                template = [
                  {
                    change_mode = "restart";
                    data = ''
                      PG_USER={{- with secret "kv/data/metadata/${namespace}" }}{{ .Data.data.pgUser }}{{ end }}
                      PG_PASS={{- with secret "kv/data/metadata/${namespace}" }}{{ .Data.data.pgPass }}{{ end }}
                    '';
                    destination = "/secrets/metadata-server-${toString instance}.sh";
                    env = true;
                  }
                ];

                config.image = ociNamer oci-images.metadata-server;
                driver = "docker";
                kill_signal = "SIGINT";
                kill_timeout = "30s";
                resources = {
                  cpu = 2000;
                  memory = memoryMb;
                };
                vault = {
                  change_mode = "noop";
                  env = true;
                  policies = ["metadata"];
                };
              };
            in {
              # ------------
              # Task: server
              # ------------
              # Server component has a memory leak which eventually leads to OOM.
              # While the server entrypoint gracefully handles re-spawn of the server task upon OOM,
              # there is a transient outage of around a dozen seconds.  Creating two server components and allowing
              # varnish to health check select between them will minimize downtime as seen from clients.
              # Varnish is used for caching and health selection as traefik caching does not have the middleware required.
              # Also, keeping these tasks in the same group prevents network hairpin failures until the cluster is migrated to Nomad >= 1.5.0
              # at which point separate taskgroups will be an option.
              server1 = mkServerTask 1;
              server2 = mkServerTask 2;

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
              # Task: varnish
              # -------------
              varnish = {
                env = {
                  VARNISH_PORT = "\${NOMAD_PORT_varnish}";
                  VARNISH_TTL_SEC = varnishTtlSec;
                  VARNISH_MALLOC_MB = varnishMemoryMb / 3;
                };

                template = [
                  {
                    change_mode = "restart";
                    data = ''
                      vcl 4.1;
                      import std;
                      import bodyaccess;
                      backend default {
                        .host = "127.0.0.1";
                        .port = "{{env "NOMAD_PORT_server1"}}";
                      }
                      acl purge {
                        "localhost";
                        "127.0.0.1";
                      }
                      sub vcl_recv {
                        unset req.http.X-Body-Len;
                        unset req.http.x-cache;
                        # Add a healthcheck
                        if (req.url == "/ping" && req.method == "GET") {
                          return(synth(700));
                        }
                        # Avoid backend calls for options
                        if (req.method == "OPTIONS") {
                          return(synth(701));
                        }
                        # Allow PURGE from localhost
                        if (req.method == "PURGE") {
                          if (!std.ip(req.http.X-Real-Ip, "0.0.0.0") ~ purge) {
                            return(synth(405,"Not Allowed"));
                          }
                          # The host is included as part of the object hash
                          # We need to match the public FQDN for the purge to be successful
                          set req.http.host = "metadata.${domain}";
                        }
                        # Allow POST caching
                        # PURGE also needs to hash the body to obtain a correct object hash to purge
                        if (req.method == "POST" || req.method == "PURGE") {
                          # Caches the body which enables POST retries if needed
                          std.cache_req_body(${toString varnishMaxPostSizeCachableKb}KB);
                          set req.http.X-Body-Len = bodyaccess.len_req_body();
                          if ((std.integer(req.http.X-Body-Len, ${toString (1024 * varnishMaxPostSizeCachableKb)}) > ${toString (1024 * varnishMaxPostSizeBodyKb)}) ||
                              (req.http.X-Body-Len == "-1")) {
                            # Uncommit to restrict rare, but possible large backend metadata requests rather than pass directly to the backend
                            # return(synth(413, "Payload Too Large"));
                            return(pass);
                          }
                          if (req.method == "PURGE") {
                            return(purge);
                          }
                          return(hash);
                        }
                      }
                      sub vcl_hash {
                        # For caching POSTs, hash the body also
                        if (req.http.X-Body-Len) {
                          bodyaccess.hash_req_body();
                        }
                        else {
                          hash_data("");
                        }
                      }
                      sub vcl_hit {
                        set req.http.x-cache = "hit";
                      }
                      sub vcl_miss {
                        set req.http.x-cache = "miss";
                      }
                      sub vcl_pass {
                        set req.http.x-cache = "pass";
                      }
                      sub vcl_pipe {
                        set req.http.x-cache = "pipe";
                      }
                      sub vcl_synth {
                        if (resp.status == 700) {
                          set resp.http.Content-Type = "text/html";
                          set resp.status = 200;
                          set resp.reason = "OK";
                          synthetic("...pong");
                          return(deliver);
                        }
                        if (resp.status == 701) {
                          set resp.http.Access-Control-Max-Age = "17280000";
                          set resp.http.Content-Type = "text/plain; charset=utf-8";
                          set resp.http.Content-Length = "0";
                          set resp.status = 204;
                          synthetic("");
                          return(deliver);
                        }
                        set req.http.x-cache = "synth synth";
                        set resp.http.x-cache = req.http.x-cache;
                      }
                      sub vcl_deliver {
                        if (obj.uncacheable) {
                          set req.http.x-cache = req.http.x-cache + " uncacheable";
                        }
                        else {
                          set req.http.x-cache = req.http.x-cache + " cached";
                        }
                        set resp.http.x-cache = req.http.x-cache;
                      }
                      sub vcl_backend_fetch {
                        if (bereq.http.X-Body-Len) {
                          set bereq.method = "POST";
                        }
                      }
                      sub vcl_backend_response {
                        if (beresp.status == 404) {
                          set beresp.ttl = ${toString (2 * varnishTtlSec / 3)}s;
                        }
                        call vcl_builtin_backend_response;
                        return(deliver);
                      }
                    '';
                    destination = "/local/default.vcl";
                  }
                ];

                config.image = ociNamer oci-images.metadata-varnish;
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
                  MASTER_REPLICA_SRV_DNS = "_infra-database._master.service.eu-central-1.consul";
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
