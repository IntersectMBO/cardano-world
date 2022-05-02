{
  inputs,
  cell,
}: let
  inherit (inputs) nixpkgs;
  inherit (cell) entrypoints; # healthChecks;
  n2c = inputs.n2c.packages.nix2container;
in (
  let
    containers = {
      "cardano-node" = n2c.buildImage {
        name = "docker.infra.aws.iohkdev.io/cardano-node";
        tag = inputs.self.rev;
        maxLayers = 25;
        contents = [nixpkgs.bashInteractive nixpkgs.iana-etc]; # healthChecks."node-network-sync"];
        config.Cmd = [
          "${entrypoints."cardano-node"}/bin/entrypoint"
        ];
      };
    };
  in
    containers
)
