self: config:
let
  inherit (self.inputs) bitte nixpkgs openziti;
  inherit (nixpkgs) lib;
in rec {
  deployType = "awsExt";
  node_class = "equinix";
  primaryInterface = "bond0";
  role = "client";

  # Equinix TF specific attrs
  project = config.cluster.name;
  plan = lib.mkDefault "m3.large.x86";

  tags = name: [
    "Cluster:${config.cluster.name}"
    "Name:${name}"
    "UID:${config.cluster.name}-${name}"
    "Consul:client"
    "Vault:client"
    "Billing:explorer"
  ];

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

      # Disable gluster
      services.glusterfs.enable = false;

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

  baseExplorerModuleConfig = name: privateIP: environmentName: [
    (bitte + /modules/zfs-client-options.nix)
    (import ./explorer/wireguard.nix name environmentName)
    (import ./explorer/explorer.nix name privateIP)
    (import ./explorer/base-service.nix privateIP)
    ./explorer/db-sync.nix
    ({pkgs, config, ...}: {
      services.cardano-node = let
        cfg = config.services.cardano-node;
      in {
        inherit environmentName;

        # m3.large.x86 in am6 facility
        topology = builtins.toFile "topology.yaml"
          (builtins.toJSON {Producers = [{addr = "europe.relays-new.cardano-mainnet.iohk.io"; port = cfg.port; valency = 2;}];});

        # m3.large.x86 has 64 logical cpu and 256 GB RAM, allocate RAM 15% for cardano-node
        totalCpuCores = 64;
        totalMaxHeapSizeMbytes = 256 * 1024 * 0.15;
      };

      services.cardano-db-sync = {
        inherit environmentName;
        restoreSnapshot =
          "https://update-cardano-mainnet.iohk.io/cardano-db-sync/13.1/db-sync-snapshot-schema-13.1-block-8987999-x86_64.tgz";
      };

      services.explorer = {
        inherit environmentName;
        totalMachineMemoryGB = 256;
      };

      services.zfs-client-options = {
        enable = lib.mkForce true;
        enableZfsSnapshots = false;
      };
    })
  ];

  mkExplorer = name: privateIP: environmentName: extra: lib.mkMerge [
    {
      inherit deployType node_class primaryInterface role privateIP;
      equinix = {
        inherit plan project;
        tags = tags name;
      };

      modules =
        baseEquinixModuleConfig
        ++ (baseEquinixMachineConfig name)
        ++ (baseExplorerModuleConfig name privateIP environmentName);
    }
    extra
  ];

  baseExplorerGatewayModuleConfig = name: privateIP: environmentName: [
    (bitte + /modules/zfs-client-options.nix)
    (import ./explorer/explorer-gateway.nix name environmentName privateIP)
    (import ./explorer/wireguard-gateway.nix name environmentName)
    ({pkgs, config, ...}: {
      services.zfs-client-options = {
        enable = lib.mkForce true;
        enableZfsSnapshots = false;
      };
    })
  ];

  mkExplorerGateway = name: privateIP: environmentName: extra: lib.mkMerge [
    {
      inherit deployType node_class primaryInterface role privateIP;
      equinix = {
        inherit project;
        plan = "m3.small.x86";
        tags = tags name;
      };

      modules =
        baseEquinixModuleConfig
        ++ (baseEquinixMachineConfig name)
        ++ (baseExplorerGatewayModuleConfig name privateIP environmentName);
    }
    extra
  ];
}
