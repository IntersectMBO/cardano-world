{
  inputs,
  cell,
}: let
  inherit (inputs) capsules bitte-cells bitte deploy-rs nixpkgs;
  inherit (inputs.std) std;
  inherit (inputs.std.lib) dev;
  inherit (inputs.cells) cardano;

  # FIXME: this is a work around just to get access
  # to 'awsAutoScalingGroups'
  # TODO: std ize bitte properly to make this interface nicer
  bitte' = inputs.bitte.lib.mkBitteStack {
    inherit inputs;
    inherit (inputs) self;
    domain = "world.dev.cardano.org";
    bitteProfile = inputs.cells.metal.bitteProfile.default;
    hydrationProfile = inputs.cells.cloud.hydrationProfiles.default;
    deploySshKey = "not-a-key";
  };

  cardanoWorld = {
    extraModulesPath,
    pkgs,
    ...
  }: {
    name = nixpkgs.lib.mkForce "Cardano World";
    imports = [
      std.devshellProfiles.default
      bitte.devshellModule
    ];
    bitte = {
      domain = "world.dev.cardano.org";
      cluster = "cardano";
      namespace = "preview";
      provider = "AWS";
      cert = null;
      aws_profile = "cardano";
      aws_region = "eu-central-1";
      aws_autoscaling_groups =
        bitte'.clusters.cardano._proto.config.cluster.awsAutoScalingGroups;
    };
  };

  # To ensure the hashistack bins in use on cli are version matched to deployed binary versions.
  # Otherwise, the default devshell-capsules are not necessarily version consistent with what is deployed across all clusters.
  # Utilized where capsules.cloud is included in the devshell.
  commands = [
    {category = "hashibins"; package = nixpkgs.lib.hiPrio bitte.legacyPackages.x86_64-linux.consul;}
    {category = "hashibins"; package = nixpkgs.lib.hiPrio bitte.legacyPackages.x86_64-linux.nomad;}
    {category = "hashibins"; package = nixpkgs.lib.hiPrio bitte.legacyPackages.x86_64-linux.vault-bin;}
  ];
in {
  dev = dev.mkShell {
    inherit commands;
    imports = [
      cardanoWorld
      capsules.base
      capsules.cloud
      capsules.integrations
      inputs.cells.cardano.devshellProfiles.dev
      inputs.cells.cardano.devshellProfiles.world
    ];
  };
  devops = dev.mkShell {
    inherit commands;
    imports = [
      cardanoWorld
      capsules.base
      capsules.cloud
      capsules.integrations
      inputs.cells.cardano.devshellProfiles.world
    ];
  };
  ops = dev.mkShell {
    imports = [
      cardanoWorld
      capsules.base
      capsules.cloud
      capsules.metal
      capsules.integrations
      capsules.tools
      bitte-cells.patroni.devshellProfiles.default
      inputs.cells.cardano.devshellProfiles.world
    ];
    commands = commands ++ [
      {category = "metal"; package = deploy-rs.defaultPackage;}
    ];
  };
  monorepo = dev.mkShell {
    imports = [
      cardanoWorld
      inputs.cells.cardano.devshellProfiles.monorepo
    ];
  };
  minimal = dev.mkShell {
    imports = [
      cardanoWorld
      inputs.cells.cardano.devshellProfiles.minimal
    ];
  };
}
