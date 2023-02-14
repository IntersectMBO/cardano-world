{
  inputs,
  cell,
}: let
  inherit (inputs) cells bitte-cells;
in {
  # Bitte Hydrate Module
  # -----------------------------------------------------------------------
  default = {bittelib, ...}: {
    imports = [
      (bitte-cells.patroni.hydrationProfiles.hydrate-cluster ["infra"])
      (cells.cardano.hydrationProfiles.workload-policies-cardano)
      (cells.cardano.hydrationProfiles.workload-policies-ogmios)
      (cells.cardano.hydrationProfiles.workload-policies-db-sync)
      (cells.cardano.hydrationProfiles.workload-policies-faucet)
      (cells.cardano.hydrationProfiles.workload-policies-wallet)
      (cells.cardano.hydrationProfiles.workload-policies-submit-api)
    ];
    # NixOS-level hydration
    # --------------
    cluster = {
      name = "cardano";
      adminNames = [
        "samuel.leathers"
        "david.arnold"
        "john.lotoski"
      ];
      developerGithubNames = [];
      developerGithubTeamNames = ["cardano-devs"];
      domain = "world.dev.cardano.org";
      extraAcmeSANs = [];
      kms = "arn:aws:kms:eu-central-1:052443713844:key/c1d7a205-5d3d-4ca7-8842-9f7fb2ccc847";
      s3Bucket = "iog-cardano-bitte";
      s3Tempo = "iog-cardano-tempo";
    };
    services = {
      nomad.namespaces = {
        infra.description = "Shared Services for The Cardano World";
        mainnet.description = "Cardano Main Network";
        shelley-qa.description = "Cardano Shelley Internal QA";
        vasil-dev.description = "Cardano Vasil HF Development Testnet";
        preprod.description = "Cardano Pre-Production Environment";
        preview.description = "Cardano Preview Environment";
        pv8.description = "Cardano Mixed Environment";
        private.description = "Cardano Private Testing Environment";
        perf.description = "Cardano Performance Testing Environment";
      };
    };

    # cluster level (terraform)
    # --------------
    tf.hydrate-cluster.configuration = {
      # ... operator role policies
      locals.policies = {
        consul.developer = {
          service_prefix."mainnet-" = {
            policy = "write";
            intentions = "write";
          };
          service_prefix."shelley-qa-" = {
            policy = "write";
            intentions = "write";
          };
          service_prefix."vasil-dev-" = {
            policy = "write";
            intentions = "write";
          };
          service_prefix."preprod-" = {
            policy = "write";
            intentions = "write";
          };
          service_prefix."preview-" = {
            policy = "write";
            intentions = "write";
          };
          service_prefix."pv8-" = {
            policy = "write";
            intentions = "write";
          };
          service_prefix."private-" = {
            policy = "write";
            intentions = "write";
          };
          service_prefix."perf-" = {
            policy = "write";
            intentions = "write";
          };
        };

        nomad.admin = {
          namespace."*".policy = "write";
          host_volume."*".policy = "write";
        };

        nomad.developer = {
          namespace.mainnet = {
            policy = "write";
            capabilities = [
              "submit-job"
              "dispatch-job"
              "read-logs"
              "alloc-exec"
              "alloc-node-exec"
              "alloc-lifecycle"
            ];
          };
          namespace.shelley-qa = {
            policy = "write";
            capabilities = [
              "submit-job"
              "dispatch-job"
              "read-logs"
              "alloc-exec"
              "alloc-node-exec"
              "alloc-lifecycle"
            ];
          };
          namespace.vasil-dev = {
            policy = "write";
            capabilities = [
              "submit-job"
              "dispatch-job"
              "read-logs"
              "alloc-exec"
              "alloc-node-exec"
              "alloc-lifecycle"
            ];
          };
          namespace.preprod = {
            policy = "write";
            capabilities = [
              "submit-job"
              "dispatch-job"
              "read-logs"
              "alloc-exec"
              "alloc-node-exec"
              "alloc-lifecycle"
            ];
          };
          namespace.preview = {
            policy = "write";
            capabilities = [
              "submit-job"
              "dispatch-job"
              "read-logs"
              "alloc-exec"
              "alloc-node-exec"
              "alloc-lifecycle"
            ];
          };
          namespace.pv8 = {
            policy = "write";
            capabilities = [
              "submit-job"
              "dispatch-job"
              "read-logs"
              "alloc-exec"
              "alloc-node-exec"
              "alloc-lifecycle"
            ];
          };
          namespace.private = {
            policy = "write";
            capabilities = [
              "submit-job"
              "dispatch-job"
              "read-logs"
              "alloc-exec"
              "alloc-node-exec"
              "alloc-lifecycle"
            ];
          };
          namespace.perf = {
            policy = "write";
            capabilities = [
              "submit-job"
              "dispatch-job"
              "read-logs"
              "alloc-exec"
              "alloc-node-exec"
              "alloc-lifecycle"
            ];
          };
          host_volume."mainnet-*".policy = "write";
          host_volume."shelley-qa-*".policy = "write";
          host_volume."vasil-dev-*".policy = "write";
          host_volume."preprod-*".policy = "write";
          host_volume."preview-*".policy = "write";
          host_volume."pv8-*".policy = "write";
          host_volume."private-*".policy = "write";
          host_volume."perf-*".policy = "write";
        };
      };
    };
    # Observability State
    # --------------
    tf.hydrate-monitoring.configuration = {
      resource =
        inputs.bitte-cells._utils.library.mkMonitoring
        # Alerts
        {
          inherit
            (cells.cardano.alerts)
            node
            dbsync
            faucet
            metal-explorer
            ;
          # Upstream alerts
          inherit
            (inputs.bitte-cells.bitte.alerts)
            bitte-consul
            bitte-deadmanssnitch
            bitte-loki
            bitte-system
            bitte-vault
            bitte-vm-health
            bitte-vm-standalone
            bitte-vmagent
            ;

          inherit
            (inputs.bitte-cells.patroni.alerts)
            bitte-cells-patroni
            ;
        }
        # Dashboards
        {
          inherit
            (cells.cardano.dashboards)
            application-metrics
            dbsync
            faucet
            p2p
            performance
            ;
          inherit
            (inputs.bitte-cells.bitte.dashboards)
            bitte-consul
            bitte-log
            bitte-loki
            bitte-nomad
            bitte-system
            bitte-traefik
            bitte-vault
            bitte-vmagent
            bitte-vmalert
            bitte-vm
            bitte-vulnix
            ;
          inherit
            (inputs.bitte-cells.patroni.dashboards)
            bitte-cells-patroni
            ;
        };
    };

    # application state (terraform)
    # --------------
    tf.hydrate-app.configuration = let
      vault' = {
        dir = ./. + "/kv/vault";
        prefix = "kv";
      };
      consul' = {
        dir = ./. + "/kv/consul";
        prefix = "config";
      };
      vault = bittelib.mkVaultResources {inherit (vault') dir prefix;};
      consul = bittelib.mkConsulResources {inherit (consul') dir prefix;};
    in {
      data = {inherit (vault) sops_file;};
      resource = {
        inherit (vault) vault_generic_secret;
        inherit (consul) consul_keys;
      };
    };
  };
}
