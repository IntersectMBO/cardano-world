{
  self,
  pkgs,
  lib,
  config,
  etcEncrypted,
  ...
}: let
  inherit (auxConfig.mainnet) explorerActiveBackends;

  auxConfig = import ./explorer/aux-config.nix self.inputs;

  # Obtain the explorer name index number
  explorerNum = backendName: builtins.elemAt (lib.splitString "-" backendName) 1;
in {
  # No longer effective; needs manual purging until a monitoring PR update
  # services.loki.configuration.table_manager = {
  #   retention_deletes_enabled = true;
  #   retention_period = "28d";
  # };

  # Change to true if/when we want tempo enabled for this cluster.
  # See also corresponding tempo option on the routing server.
  services.monitoring.useTempo = false;

  services.prometheus.exporters.blackbox = lib.mkForce {
    enable = true;
    configFile = pkgs.toPrettyJSON "blackbox-exporter.yaml" {
      modules = rec {
        http_2xx = {
          prober = "http";
          timeout = "10s";
        };
        https_2xx = http_2xx // {http.fail_if_not_ssl = true;};
        https_explorer_post_2xx = lib.recursiveUpdate https_2xx {
          http = {
            method = "POST";
            headers.Content-Type = "application/json";
            body = ''{"query": "{\n  ada {\n    supply {\n      total\n    }\n  }\n}\n"}'';
          };
        };
      };
    };
  };

  # Monitor the mainnet explorers and explorer gateway
  services.vmagent.promscrapeConfig = let
    environment = "mainnet";
    labels = machine: rec {
      alias = machine;

      # Adding these labels allows re-use of nomad-dashboards under a metal-* namespace
      nomad_alloc_name = alias;
      namespace = "metal-explorer-${environment}";
    };

    relabel_configs = [
      {
        source_labels = ["__address__"];
        target_label = "__param_target";
      }
      {
        source_labels = ["__param_target"];
        target_label = "instance";
      }
      {
        replacement = "127.0.0.1:9115";
        target_label = "__address__";
      }
    ];

    mkTarget = ip: port: machine: {
      labels = labels machine;
      targets = ["${ip}:${toString port}"];
    };

    mkBlackboxTarget = target: machine: {
      labels = labels machine;
      targets = [target];
    };

    mkExplorerGatewayTargets = explorerTargetList: [
      {
        job_name = "explorer-exporter-gateway";
        scrape_interval = "60s";
        metrics_path = "/metrics";
        static_configs = lib.flatten (map (attr: map (port: mkTarget attr.ip port attr.machine) [9100 9586]) explorerTargetList);
      }
      {
        job_name = "blackbox-explorer-frontend";
        scrape_interval = "60s";
        metrics_path = "/probe";
        params.module = ["https_2xx"];
        static_configs = [{targets = ["https://explorer.cardano.org"]; labels = {};}];
        inherit relabel_configs;
      }
      {
        job_name = "blackbox-explorer-graphql";
        scrape_interval = "60s";
        metrics_path = "/probe";
        params.module = ["https_explorer_post_2xx"];
        static_configs = [{targets = ["https://explorer.cardano.org/graphql"]; labels = {};}];
        inherit relabel_configs;
      }
    ];

    mkExplorerTargets = explorerTargetList: [
      {
        job_name = "explorer-exporter";
        scrape_interval = "60s";
        metrics_path = "/metrics";
        static_configs = lib.flatten (map (attr: map (port: mkTarget attr.ip port attr.machine) [9100 9131 9586 12798]) explorerTargetList);
      }
      {
        job_name = "explorer-nginx";
        scrape_interval = "60s";
        metrics_path = "/status/format/prometheus";
        static_configs = map (attr: mkTarget attr.ip 9113 attr.machine) explorerTargetList;
      }
      {
        job_name = "explorer-dbsync";
        scrape_interval = "60s";
        metrics_path = "/";
        static_configs = map (attr: mkTarget attr.ip 8080 attr.machine) explorerTargetList;
      }
      {
        job_name = "explorer-topology";
        scrape_interval = "60s";
        metrics_path = "/";
        static_configs = map (attr: mkTarget attr.ip 8888 attr.machine) explorerTargetList;
      }
      {
        job_name = "blackbox-explorer-graphql-healthcheck";
        scrape_interval = "60s";
        metrics_path = "/probe";
        params.module = ["http_2xx"];
        static_configs = map (attr: mkBlackboxTarget "http://${attr.ip}:9999/healthz" attr.machine) explorerTargetList;
        inherit relabel_configs;
      }
    ];

    inherit (auxConfig.${environment}) explorerActiveBackends;
  in (
    (mkExplorerGatewayTargets [
      {ip = config.cluster.awsExtNodes.explorer.privateIP; machine = "explorer";}
    ])
    ++
    (mkExplorerTargets (map (backend: {ip = config.cluster.awsExtNodes.${backend.name}.privateIP; machine = backend.name;}) explorerActiveBackends))
  );

  networking = {
    # See the bitte profiles/multicloud/equinix.nix for equinix firewall handling
    firewall.allowedUDPPorts = lib.mkForce [51820];
    wireguard = {
      enable = true;
      interfaces.wg = {
        listenPort = 51820;
        ips = ["192.168.254.100/32"];
        privateKeyFile = "/etc/wireguard/private.key";
        peers = (map (backend: {
          inherit (backend) publicKey;
          allowedIPs = ["192.168.254.${explorerNum backend.name}/32"];
          endpoint = "${config.cluster.awsExtNodes.${backend.name}.privateIP}:51820";
          persistentKeepalive = 30;
        }) explorerActiveBackends) ++ [
          # cardano-world explorer gateway
          # wg pubkey < <(sops -d ../encrypted/wg/explorer-private)
          {
            publicKey = "4DEOtdKOu8h284ZwjOsd/cKqSmuQnI+Jy2yiUPxG9B8=";
            allowedIPs = ["192.168.254.254/32"];
            endpoint = "${config.cluster.awsExtNodes.explorer.privateIP}:51820";
            persistentKeepalive = 30;
          }
        ];
      };
    };
  };

  secrets.install.wg-private = {
    source = "${etcEncrypted}/wg/monitoring-private";
    target = "/etc/wireguard/private.key";
    outputType = "binary";
    extraPackages = [pkgs.wireguard-tools];
    script = ''
      chmod 0400 /etc/wireguard/private.key
      wg pubkey < /etc/wireguard/private.key > /etc/wireguard/public.key
    '';
  };
}
