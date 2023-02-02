{
  inputs,
  cell,
}: let
  inherit (inputs) openziti nixpkgs;
in {
  default = {
    self,
    lib,
    pkgs,
    config,
    terralib,
    bittelib,
    ...
  }: let
    inherit (self.inputs) bitte;
    inherit (config) cluster;
    sr = {
      inherit
        (bittelib.securityGroupRules config)
        internet
        internal
        ssh
        http
        https
        routing
        ziti-controller-mgmt
        ziti-controller-rest
        ziti-router-edge
        ziti-router-fabric
        ;
    };
  in {
    secrets.encryptedRoot = ./encrypted;

    cluster = {
      s3CachePubKey = lib.fileContents ./encrypted/nix-public-key-file;
      flakePath = "${inputs.self}";
      vbkBackend = "local";
      infraType = "awsExt";
      transitGateway = {
        enable = true;
        transitRoutes = [
          # Equinix, cardano project
          {
            gatewayCoreNodeName = "zt";
            cidrRange = "10.12.171.0/24";
          }
        ];
      };

      autoscalingGroups = let
        defaultModules = [(bitte + "/profiles/client.nix")];

        eachRegion = attrs: [
          (attrs // {region = "eu-central-1";})
          (attrs // {region = "eu-west-1";})
          (attrs // {region = "us-east-2";})
        ];
        euCentral = attrs: [
          (attrs // {region = "eu-central-1";})
        ];
      in
        lib.listToAttrs
        (
          lib.forEach
          (
            # Infra Nodes
            (euCentral {
              instanceType = "t3.2xlarge";
              desiredCapacity = 3;
              volumeSize = 500;
              modules =
                defaultModules
                ++ [
                  (
                    bittelib.mkNomadHostVolumesConfig
                    [
                      "infra-database"
                    ]
                    (n: "/var/lib/nomad-volumes/${n}")
                  )
                  # for scheduling constraints
                  {services.nomad.client.meta.patroni = "yeah";}
                ];
              node_class = "infra";
            })
            ++
            # Vasil-QA nodes
            (eachRegion {
              instanceType = "t3.2xlarge";
              desiredCapacity = 6;
              volumeSize = 500;
              modules =
                defaultModules
                ++ [
                  (
                    bittelib.mkNomadHostVolumesConfig
                    ["mainnet-persist-cardano-node-local"]
                    (n: "/var/lib/nomad-volumes/${n}")
                  )
                  (
                    bittelib.mkNomadHostVolumesConfig
                    ["mainnet-persist-db-sync-local"]
                    (n: "/mnt/gv0/${n}")
                  )
                  (
                    bittelib.mkNomadHostVolumesConfig
                    ["shelley-qa-persist-cardano-node-local"]
                    (n: "/var/lib/nomad-volumes/${n}")
                  )
                  (
                    bittelib.mkNomadHostVolumesConfig
                    ["shelley-qa-persist-db-sync-local"]
                    (n: "/mnt/gv0/${n}")
                  )
                  (
                    bittelib.mkNomadHostVolumesConfig
                    ["vasil-dev-persist-cardano-node-local"]
                    (n: "/var/lib/nomad-volumes/${n}")
                  )
                  (
                    bittelib.mkNomadHostVolumesConfig
                    ["vasil-dev-persist-db-sync-local"]
                    (n: "/mnt/gv0/${n}")
                  )
                  (
                    bittelib.mkNomadHostVolumesConfig
                    ["preprod-persist-cardano-node-local"]
                    (n: "/var/lib/nomad-volumes/${n}")
                  )
                  (
                    bittelib.mkNomadHostVolumesConfig
                    ["preprod-persist-db-sync-local"]
                    (n: "/mnt/gv0/${n}")
                  )
                  (
                    bittelib.mkNomadHostVolumesConfig
                    ["preview-persist-cardano-node-local"]
                    (n: "/var/lib/nomad-volumes/${n}")
                  )
                  (
                    bittelib.mkNomadHostVolumesConfig
                    ["preview-persist-db-sync-local"]
                    (n: "/mnt/gv0/${n}")
                  )
                  (
                    bittelib.mkNomadHostVolumesConfig
                    ["pv8-persist-cardano-node-local"]
                    (n: "/var/lib/nomad-volumes/${n}")
                  )
                  (
                    bittelib.mkNomadHostVolumesConfig
                    ["private-persist-cardano-node-local"]
                    (n: "/var/lib/nomad-volumes/${n}")
                  )
                  (
                    bittelib.mkNomadHostVolumesConfig
                    ["private-persist-db-sync-local"]
                    (n: "/mnt/gv0/${n}")
                  )
                  # for scheduling constraints
                  {services.nomad.client.meta.cardano = "yeah";}
                ];
              node_class = "qa";
            })
          )
          (args: let
            attrs =
              {
                desiredCapacity = 6;
                instanceType = "t3a.large";
                associatePublicIP = true;
                maxInstanceLifetime = 0;
                iam.role = cluster.iam.roles.client;
                iam.instanceProfile.role = cluster.iam.roles.client;

                securityGroupRules = {inherit (sr) internet internal ssh;};
              }
              // args;
            asgName = "client-${attrs.region}-${
              builtins.replaceStrings [''.''] [''-''] attrs.instanceType
            }-${args.node_class}";
          in
            lib.nameValuePair asgName attrs)
        );

      instances = {
        core-1 = {
          instanceType = "t3a.medium";
          privateIP = "172.16.0.10";
          subnet = cluster.vpc.subnets.core-1;
          volumeSize = 100;

          modules = [
            (bitte + /profiles/core.nix)
            (bitte + /profiles/bootstrapper.nix)
          ];

          securityGroupRules = {inherit (sr) internet internal ssh;};
        };

        core-2 = {
          instanceType = "t3a.medium";
          privateIP = "172.16.1.10";
          subnet = cluster.vpc.subnets.core-2;
          volumeSize = 100;

          modules = [
            (bitte + /profiles/core.nix)
          ];

          securityGroupRules = {inherit (sr) internet internal ssh;};
        };

        core-3 = {
          instanceType = "t3a.medium";
          privateIP = "172.16.2.10";
          subnet = cluster.vpc.subnets.core-3;
          volumeSize = 100;

          modules = [
            (bitte + /profiles/core.nix)
          ];

          securityGroupRules = {inherit (sr) internet internal ssh;};
        };

        monitoring = {
          instanceType = "t3a.xlarge";
          privateIP = "172.16.0.20";
          subnet = cluster.vpc.subnets.core-1;
          volumeSize = 500;
          securityGroupRules = {inherit (sr) internet internal ssh http https;};
          modules = [
            (bitte + /profiles/monitoring.nix)
            {
              # No longer effective; needs manual purging until a monitoring PR update
              # services.loki.configuration.table_manager = {
              #   retention_deletes_enabled = true;
              #   retention_period = "28d";
              # };

              # Change to true if/when we want tempo enabled for this cluster.
              # See also corresponding tempo option on the routing server.
              services.monitoring.useTempo = false;
            }
          ];
        };

        routing = {
          instanceType = "t3a.small";
          privateIP = "172.16.1.20";
          subnet = cluster.vpc.subnets.core-2;
          volumeSize = 30;
          securityGroupRules = {inherit (sr) internet internal ssh http https routing;};
          route53.domains = [
            "*.${cluster.domain}"
            "consul.${cluster.domain}"
            "docker.${cluster.domain}"
            "monitoring.${cluster.domain}"
            "nomad.${cluster.domain}"
            "vault.${cluster.domain}"
          ];

          modules = [
            (bitte + /profiles/routing.nix)
            {
              services.oauth2_proxy.email.domains = ["iohk.io"];

              # Change to true if/when we want tempo enabled for this cluster.
              # See also corresponding tempo option on the monitoring server.
              services.traefik.enableTracing = false;

              services.traefik.staticConfigOptions = {
                entryPoints =
                  lib.pipe {
                    preprod = 30000;
                    preview = 30002;
                    shelley-qa = 30003;
                    vasil-dev = 30001;
                    pv8 = 30006;
                    private = 30007;
                  } [
                    (
                      lib.mapAttrsToList (
                        namespace: port: {
                          name = "${namespace}-node-tcp";
                          value.address = ":${toString port}";
                        }
                      )
                    )
                    lib.listToAttrs
                  ];
              };
            }
          ];
        };

        # GlusterFS storage nodes
        storage-0 = {
          instanceType = "t3a.small";
          privateIP = "172.16.0.30";
          subnet = config.cluster.vpc.subnets.core-1;
          volumeSize = 40;
          modules = [(bitte + /profiles/storage.nix)];
          securityGroupRules = {inherit (sr) internal internet ssh;};
          ebsVolume = {
            iops = 3000; # 3000..16000
            size = 500; # GiB
            type = "gp3";
            throughput = 125; # 125..1000 MiB/s
          };
        };

        storage-1 = {
          instanceType = "t3a.small";
          privateIP = "172.16.1.30";
          subnet = config.cluster.vpc.subnets.core-2;
          volumeSize = 40;
          modules = [(bitte + /profiles/storage.nix)];
          securityGroupRules = {inherit (sr) internal internet ssh;};
          ebsVolume = {
            iops = 3000; # 3000..16000
            size = 500; # GiB
            type = "gp3";
            throughput = 125; # 125..1000 MiB/s
          };
        };

        storage-2 = {
          instanceType = "t3a.small";
          privateIP = "172.16.2.30";
          subnet = config.cluster.vpc.subnets.core-3;
          volumeSize = 40;
          modules = [(bitte + /profiles/storage.nix)];
          securityGroupRules = {inherit (sr) internal internet ssh;};
          ebsVolume = {
            iops = 3000; # 3000..16000
            size = 500; # GiB
            type = "gp3";
            throughput = 125; # 125..1000 MiB/s
          };
        };

        zt = {
          # https://support.netfoundry.io/hc/en-us/articles/360025875331-Edge-Router-VM-Sizing-Guide
          instanceType = "c5.large";
          privateIP = "172.16.0.100";
          subnet = cluster.vpc.subnets.core-1;
          volumeSize = 100;
          route53.domains = ["zt.${cluster.domain}"];
          sourceDestCheck = false;

          modules = [
            inputs.bitte.profiles.common
            inputs.bitte.profiles.consul-common
            inputs.bitte.profiles.vault-cache
            openziti.nixosModules.ziti-controller
            openziti.nixosModules.ziti-router
            openziti.nixosModules.ziti-console
            openziti.nixosModules.ziti-edge-tunnel
            ./ziti.nix
            ./ziti-register.nix
          ];

          securityGroupRules = {
            inherit
              (sr)
              internal
              internet
              ssh
              ziti-controller-mgmt
              ziti-controller-rest
              ziti-router-edge
              ziti-router-fabric
              ;
          };
        };
      };

      awsExtNodes = let
        # For each new machine provisioning to equinix:
        #   1) TF plan/apply in the `equinix` workspace to get the initial machine provisioning done after declaration
        #      `nix run .#clusters.cardano.tf.equinix.[plan|apply]
        #   2) Record the privateIP attr that the machine is assigned in the nix metal code
        #   3) Add the provisioned machine to ssh config for deploy-rs to utilize
        #   4) Update the encrypted ssh config file with the new machine so others can easily pull the ssh config
        #   5) Deploy again with proper private ip, provisioning configuration and bitte stack modules applied
        #      `deploy -s .#$CLUSTER_NAME-$MACHINE_NAME --auto-rollback false --magic-rollback false
        deployType = "awsExt";
        node_class = "equinix";
        primaryInterface = "bond0";
        role = "client";

        # Equinix TF specific attrs
        project = config.cluster.name;
        plan = lib.mkDefault "m3.large.x86";

        baseEquinixMachineConfig = machineName:
          if builtins.pathExists ./equinix/${machineName}/configuration.nix
          then [./equinix/${machineName}/configuration.nix]
          else [];

        baseEquinixModuleConfig = [
          (bitte + /profiles/client.nix)
          (bitte + /profiles/multicloud/aws-extended.nix)
          (bitte + /profiles/multicloud/equinix.nix)
          openziti.nixosModules.ziti-edge-tunnel
          ({
            pkgs,
            lib,
            config,
            ...
          }: {
            services.ziti-edge-tunnel.enable = true;

            # Temporarily disable nomad to avoid conflict with buildkite resource consumption.
            services.nomad.enable = lib.mkForce false;

            services.resolved = {
              # Vault agent does not seem to recognize successful lookups while resolved is in dnssec allow-downgrade mode
              dnssec = "false";

              # Ensure dnsmasq stays as the primary resolver while resolved is in use
              extraConfig = "Domains=~.";
            };

            # Utilize swap only as a last resort
            boot.kernel.sysctl."vm.swappiness" = 1;

            # Extra prem diagnostic utils
            environment.systemPackages = with pkgs; [
              conntrack-tools
              ethtool
              icdiff
              iptstate
              tshark
            ];
          })
        ];

        baseExplorerModuleConfig = privateIP: [
          (import ./explorer/base-service.nix privateIP)
          ./explorer/db-sync.nix
          ./explorer/explorer.nix
          (bitte + /modules/zfs-client-options.nix)
          ({pkgs, config, ...}: let
            cfg = config.services.cardano-node;
          in {
            services.cardano-node = {
              # m3.large.x86 in am6 facility
              topology = builtins.toFile "topology.yaml"
                (builtins.toJSON {Producers = [{addr = "europe.relays-new.cardano-mainnet.iohk.io"; port = cfg.port; valency = 2;}];});

              # m3.large.x86 has 64 logical cpu and 256 GB RAM
              totalCpuCores = 64;
              totalMaxHeapSizeMbytes = 256 * 1024 * 0.15;
            };

            services.cardano-db-sync.restoreSnapshot =
              "https://update-cardano-mainnet.iohk.io/cardano-db-sync/13/db-sync-snapshot-schema-13-block-8291499-x86_64.tgz";

            services.zfs-client-options.enable = lib.mkForce true;
          })
        ];

        mkExplorer = name: privateIP: extra: lib.mkMerge [
          {
            inherit deployType node_class primaryInterface role privateIP;
            equinix = {inherit plan project;};

            modules =
              baseEquinixModuleConfig
              ++ (baseEquinixMachineConfig name)
              ++ (baseExplorerModuleConfig privateIP);
          }
          extra
        ];
      in {
        explorer-1 = mkExplorer "explorer-1" "10.12.171.129" {};
        # explorer-2 = mkExplorer "explorer-2" "10.12.171.131" {};
        # explorer-3 = mkExplorer "explorer-3" "10.12.171.133" {};
      };
    };
  };
}
