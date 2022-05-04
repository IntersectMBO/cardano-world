{
  inputs,
  cell,
}: let
  inherit (inputs) nixpkgs;
  inherit (cell) entrypoints; # healthChecks;
  n2c = inputs.n2c.packages.nix2container;
in {
  "cardano-node" = n2c.buildImage {
    name = "docker.infra.aws.iohkdev.io/cardano-node";
    maxLayers = 25;
    contents = [nixpkgs.bashInteractive nixpkgs.iana-etc]; # healthChecks.cardano-node-network-sync];
    config.Cmd = [
      "${entrypoints.cardano-node}/bin/entrypoint"
    ];
  };
  "cardano-wallet" = n2c.buildImage {
    name = "docker.infra.aws.iohkdev.io/cardano-wallet";
    maxLayers = 25;
    contents = [nixpkgs.bashInteractive nixpkgs.iana-etc]; # healthChecks.cardano-wallet-network-sync];
    config.Cmd = [
      "${entrypoints.cardano-wallet}/bin/entrypoint"
    ];
  };
  "cardano-db-sync" = n2c.buildImage {
    name = "docker.infra.aws.iohkdev.io/cardano-db-sync";
    maxLayers = 25;
    contents = [nixpkgs.bashInteractive nixpkgs.iana-etc]; # healthChecks.cardano-db-sync-network-sync];
    config.Cmd = [
      "${entrypoints.cardano-db-sync}/bin/entrypoint"
    ];
  };
  "cardano-submit-api" = n2c.buildImage {
    name = "docker.infra.aws.iohkdev.io/cardano-submit-api";
    maxLayers = 25;
    contents = [nixpkgs.bashInteractive nixpkgs.iana-etc];
    config.Cmd = [
      "${entrypoints.cardano-submit-api}/bin/entrypoint"
    ];
  };
}
