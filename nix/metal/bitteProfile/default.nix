{
  inputs,
  cell,
}: let
  inherit (inputs.bitte-cells) patroni cardano;
in {
  default = {
    self,
    lib,
    pkgs,
    config,
    terralib,
    bittelib,
    ...
  } @ args: let
    inherit (self.inputs) bitte;
    inherit (bittelib) mkNomadHostVolumesConfig;
    inherit (config) cluster;
    inherit (import ./security-group-rules.nix args) securityGroupRules;
  in {
    imports = [./docker-auth.nix];
    secrets.encryptedRoot = ./encrypted;
    cluster = {
      s3CachePubKey = lib.fileContents ./encrypted/nix-public-key-file;
      flakePath = "${self}";

      awsAutoScalingGroups = let
        defaultModules = [
          (bitte + "/profiles/client.nix")
          "${self.inputs.nixpkgs}/nixos/modules/profiles/headless.nix"
          ./secrets.nix
          {
            # Watchdog events will be logged but not force the nomad client node to restart
            # Comment this line out to allow forced watchdog restarts
            # Patroni HA Postgres jobs will utilize watchdog and log additional info if it's available
            boot.kernelModules = ["softdog"];
            boot.extraModprobeConfig = "options softdog soft_noboot=1";
          }
        ];

        eachRegion = attrs: [
          (attrs // {region = "eu-central-1";})
          (attrs // {region = "eu-west-1";})
          (attrs // {region = "us-east-2";})
        ];
      in
        lib.listToAttrs (
          lib.forEach (
            (eachRegion {
              volumeSize = 100;
              modules = defaultModules;
              node_class = "production";
            })
            ++ (eachRegion {
              instanceType = "t3.xlarge";
              volumeSize = 500;
              modules = defaultModules ++ [(patroni.nixosProfiles.client "infra")];
              node_class = "infra";
            })
            ++ (eachRegion {
              instanceType = "c5.4xlarge";
              volumeSize = 500;
              modules = defaultModules ++ [(cardano.nixosProfiles.client "infra")];
              node_class = "infra";
            })
          ) (args: let
            attrs =
              {
                desiredCapacity = 1;
                instanceType = "t3a.large";
                associatePublicIP = true;
                maxInstanceLifetime = 0;
                iam.role = cluster.iam.roles.client;
                iam.instanceProfile.role = cluster.iam.roles.client;

                securityGroupRules = {
                  inherit (securityGroupRules) internet internal ssh;
                };
              }
              // args;
            asgName = "client-${attrs.region}-${
              builtins.replaceStrings [''.''] [''-''] attrs.instanceType
            }-${args.node_class}";
          in
            lib.nameValuePair asgName attrs)
        );
      coreNodes = {
        core-1 = {
          instanceType = "t3a.medium";
          ami = "ami-047e751e259941f2f";
          privateIP = "172.16.0.10";
          subnet = cluster.vpc.subnets.core-1;
          volumeSize = 100;
          modules = [
            (bitte + /profiles/core.nix)
            (bitte + /profiles/bootstrapper.nix)
            ./secrets.nix
          ];
          securityGroupRules = {inherit (securityGroupRules) internet internal ssh;};
        };
        core-2 = {
          instanceType = "t3a.medium";
          ami = "ami-047e751e259941f2f";
          privateIP = "172.16.1.10";
          subnet = cluster.vpc.subnets.core-2;
          volumeSize = 100;
          modules = [
            (bitte + /profiles/core.nix)
            ./secrets.nix
          ];
          securityGroupRules = {inherit (securityGroupRules) internet internal ssh;};
        };
        core-3 = {
          instanceType = "t3a.medium";
          ami = "ami-047e751e259941f2f";
          privateIP = "172.16.2.10";
          subnet = cluster.vpc.subnets.core-3;
          volumeSize = 100;
          modules = [
            (bitte + /profiles/core.nix)
            ./secrets.nix
          ];
          securityGroupRules = {inherit (securityGroupRules) internet internal ssh;};
        };
        monitoring = {
          instanceType = "t3a.xlarge";
          ami = "ami-047e751e259941f2f";
          privateIP = "172.16.0.20";
          subnet = cluster.vpc.subnets.core-1;
          volumeSize = 300;
          modules = [
            (bitte + /profiles/monitoring.nix)
            ./secrets.nix
          ];
          securityGroupRules = {
            inherit
              (securityGroupRules)
              internet
              internal
              ssh
              http
              https
              docker-registry
              ;
          };
        };
        routing = {
          instanceType = "t3a.small";
          ami = "ami-047e751e259941f2f";
          privateIP = "172.16.1.20";
          subnet = cluster.vpc.subnets.core-2;
          volumeSize = 30;
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
            ./secrets.nix
            {
              services.traefik = {
                staticConfigOptions = {
                  entryPoints = {
                    grpc = {
                      address = ":50053";
                      forwardedHeaders.insecure = true;
                    };
                  };
                };
              };
            }
          ];
          securityGroupRules = {
            inherit
              (securityGroupRules)
              internet
              internal
              ssh
              http
              routing
              ;
          };
        };
        # GlusterFS storage nodes
        storage-0 = {
          instanceType = "t3a.small";
          privateIP = "172.16.0.30";
          ami = "ami-047e751e259941f2f";
          subnet = config.cluster.vpc.subnets.core-1;
          volumeSize = 40;
          ebsVolume = {
            iops = 3000; # 3000..16000
            size = 500; # GiB
            type = "gp3";
            throughput = 125; # 125..1000 MiB/s
          };

          modules = [
            (bitte + /profiles/storage.nix)
          ];

          securityGroupRules = {
            inherit (securityGroupRules) internal internet ssh;
          };
        };

        storage-1 = {
          instanceType = "t3a.small";
          privateIP = "172.16.1.30";
          ami = "ami-047e751e259941f2f";
          subnet = config.cluster.vpc.subnets.core-2;
          volumeSize = 40;
          ebsVolume = {
            iops = 3000; # 3000..16000
            size = 500; # GiB
            type = "gp3";
            throughput = 125; # 125..1000 MiB/s
          };

          modules = [(bitte + /profiles/storage.nix)];

          securityGroupRules = {
            inherit (securityGroupRules) internal internet ssh;
          };
        };

        storage-2 = {
          instanceType = "t3a.small";
          privateIP = "172.16.2.30";
          ami = "ami-047e751e259941f2f";
          subnet = config.cluster.vpc.subnets.core-3;
          volumeSize = 40;
          ebsVolume = {
            iops = 3000; # 3000..16000
            size = 500; # GiB
            type = "gp3";
            throughput = 125; # 125..1000 MiB/s
          };

          modules = [(bitte + /profiles/storage.nix)];

          securityGroupRules = {
            inherit (securityGroupRules) internal internet ssh;
          };
        };
      };
    };
  };
}
