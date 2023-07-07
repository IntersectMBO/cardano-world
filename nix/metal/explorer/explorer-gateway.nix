name: environmentName: privateIP: {
  self,
  pkgs,
  lib,
  config,
  etcEncrypted,
  ...
}: let
  inherit (lib) mkOption types;
  inherit
    (auxConfig.${environmentName})
    explorerHostName
    explorerAliases
    explorerActiveBackends
    explorerRosettaActiveBackends
    cardanoExplorerGwPrometheusExporterPort
    smashHostName
    ;
  auxConfig = import ./aux-config.nix self.inputs;

  # Obtain the explorer name index number
  explorerNum = backendName: builtins.elemAt (lib.splitString "-" backendName) 1;
in {
  # For bitte telegraf profile compatability
  options = {
    services.traefik.prometheusPort = mkOption {
      type = types.int;
      default = cardanoExplorerGwPrometheusExporterPort;
    };
  };

  config = {
    networking = {
      extraHosts =
        lib.concatMapStringsSep "\n"
        (backend: "192.168.254.${explorerNum backend.name} ${backend.name}")
        explorerActiveBackends;

      # See the bitte profiles/multicloud/equinix.nix for equinix firewall handling
      firewall = {
        allowedTCPPorts = lib.mkForce [80 443];
        extraCommands = ''
          # Allow scrapes for metrics to the private IP from the monitoring server
          iptables -A nixos-fw -d ${privateIP}/32 -p tcp --dport 9100 -m comment --comment "node-exporter metrics exporter" -j nixos-fw-accept
          iptables -A nixos-fw -d ${privateIP}/32 -p tcp --dport 9586 -m comment --comment "wireguard metrics exporter" -j nixos-fw-accept
        '';
      };
    };

    # Reduce default interval to allow for more restarts (5 in 1 min) before it fails
    systemd.services.traefik.startLimitIntervalSec = lib.mkForce 60;

    services.traefik = {
      enable = true;
      prometheusPort = cardanoExplorerGwPrometheusExporterPort;

      staticConfigOptions = {
        accesslog = true;
        api.dashboard = false;
        log.level = "info";
        metrics.prometheus.entryPoint = "metrics";

        entryPoints = {
          web = {
            address = ":80";
            http = {
              redirections = {
                entryPoint = {
                  to = "websecure";
                  scheme = "https";
                };
              };
            };
          };
          websecure = {
            address = ":443";
          };
          metrics = {
            address = ":${toString cardanoExplorerGwPrometheusExporterPort}";
          };
        };
        certificatesResolvers.default.acme = {
          email = "devops@iohk.io";
          storage = "/var/lib/traefik/acme.json";
          httpChallenge = {
            entryPoint = "web";
          };
        };
      };

      dynamicConfigOptions = let
        allExplorerHostnames = lib.concatStringsSep " || " (map (alias: "Host(`${alias}`)") ([explorerHostName] ++ explorerAliases));
      in {
        http = {
          routers = {
            # Router rule priority by default is from longest to shortest
            graphql = {
              rule = "(${allExplorerHostnames}) && PathPrefix(`/graphql`)";
              service = "graphql";
              middlewares = ["graphqlRateLimit"];
              tls.certResolver = "default";
            };

            rosetta = {
              rule = "(${allExplorerHostnames}) && PathPrefix(`/rosetta`)";
              service = "rosetta";
              tls.certResolver = "default";
            };

            explorer = {
              rule = allExplorerHostnames;
              service = "explorer";
              tls.certResolver = "default";
            };

            smash = {
              rule = "Host(`${smashHostName}`)";
              service = "smash";
              tls.certResolver = "default";
            };
          };

          middlewares = {
            graphqlRateLimit = {
               # Apply a 3 minute rate limit window averaging 0.5 requests/second with up to 30 req burst
               # These params should be loose enough to allow typical UI usage while avoiding 429s.
               rateLimit = {
                 average = 90;
                 burst = 30;
                 period = "180s";
               };
            };
          };

          services = {
            explorer = {
              loadBalancer = {
                healthCheck = {
                  path = "/supply/total";
                  interval = "60s";
                  timeout = "10s";
                };

                servers = map (backend: {
                  url = "http://${backend.name}";
                }) explorerActiveBackends;
              };
            };

            graphql = {
              loadBalancer = {
                healthCheck = {
                  path = "/healthz";
                  port = 9999;
                  interval = "60s";
                  timeout = "10s";
                };

                servers = map (backend: {
                  url = "http://${backend.name}";
                }) explorerActiveBackends;
              };
            };

            rosetta = {
              loadBalancer = {
                healthCheck = {
                  path = "/rosetta/health";
                  interval = "60s";
                  timeout = "10s";
                };

                servers = map (backend: {
                  url = "http://${backend}";
                }) explorerRosettaActiveBackends;
              };
            };

            smash = {
              loadBalancer = {
                healthCheck = {
                  path = "/api/v1/status";
                  interval = "60s";
                  timeout = "10s";
                };

                servers = map (backend: {
                  url = "http://${backend.name}:81";
                }) explorerActiveBackends;
              };
            };
          };
        };
      };
    };

    services.prometheus.exporters = {
      # Firewall handling is done in the networking.firewall.extraCommands block above
      node = {
        enable = true;
        listenAddress = privateIP;
        port = 9100;
        enabledCollectors = [
          "bonding"
          "conntrack"
          "cpu"
          "cpufreq"
          "diskstats"
          "edac"
          "entropy"
          "filefd"
          "filesystem"
          "ksmd"
          "loadavg"
          "logind"
          "meminfo"
          "netdev"
          "netstat"
          "processes"
          "pressure"
          "nvme"
          "sockstat"
          "stat"
          "systemd"
          "tcpstat"
          "time"
          "timex"
          "vmstat"
        ];
      };

      wireguard = {
        enable = true;
        listenAddress = privateIP;
        port = 9586;
      };
    };
  };
}
