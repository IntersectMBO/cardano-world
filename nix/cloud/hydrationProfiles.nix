{
  inputs,
  cell,
}: let
  inherit (inputs) cells bitte-cells;
  inherit (inputs.nixpkgs) lib;
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
      (cells.cardano.hydrationProfiles.workload-policies-metadata)
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
        preprod.description = "Cardano Pre-Production Environment";
        preview.description = "Cardano Preview Environment";
        private.description = "Cardano Private Testing Environment";
        perf.description = "Cardano Performance Testing Environment";
      };
    };

    # cluster level (terraform)
    # --------------
    tf.hydrate-cluster.configuration = {
      resource.vault_github_team.performance-tracing = {
        backend = "\${vault_github_auth_backend.employee.path}";
        team = "performance-tracing";
        policies = ["perf"];
      };

      # ... operator role policies
      locals.policies = {
        vault = let
          c = "create";
          r = "read";
          u = "update";
          d = "delete";
          l = "list";
          s = "sudo";
          caps = lib.mapAttrs (n: v: {capabilities = v;});
        in {
          perf.path = caps {
            "auth/token/lookup" = [u];
            "auth/token/lookup-self" = [r];
            "auth/token/renew-self" = [u];
            "sys/capabilities-self" = [u];
            "kv/data/postgrest/*" = [r l];
            "kv/metadata/postgrest/*" = [r l];
            "nomad/creds/perf" = [r u];
            "consul/creds/developer" = [r u];
            "sops/keys/dev" = [r l];
            "sops/decrypt/dev" = [r u l];
            "sops/encrypt/dev" = [r u l];
          };
        };

        consul.developer = {
          service_prefix."mainnet-" = {
            policy = "write";
            intentions = "write";
          };
          service_prefix."shelley-qa-" = {
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
          host_volume."mainnet-*".policy = "write";
          host_volume."shelley-qa-*".policy = "write";
          host_volume."preprod-*".policy = "write";
          host_volume."preview-*".policy = "write";
          host_volume."private-*".policy = "write";
        };

        nomad.perf = {
          description = "Performance tracing and benchmarking policies";
          namespace."*".policy = "deny";
          namespace."perf" = {
            policy = "write";
            capabilities = [
              "alloc-exec"
              "alloc-lifecycle"
              "alloc-node-exec"
              "dispatch-job"
              "list-jobs"
              "list-scaling-policies"
              "read-fs"
              "read-job"
              "read-job-scaling"
              "read-logs"
              "read-scaling-policy"
              "scale-job"
              "submit-job"
            ];
          };
          host_volume."perf-*".policy = "write";
          node.policy = "read";
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
            nginx-basic
            nginx-vts
            node-exporter
            p2p
            performance
            varnish
            wireguard
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
