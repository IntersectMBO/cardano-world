{
  inputs,
  cell,
}: let
  inherit (inputs) nixpkgs;
  inherit (inputs.bitte-cells) _utils;
  inherit (cell) entrypoints packages healthChecks;
  n2c = inputs.n2c.packages.nix2container;

  buildDebugImage = ep: o: n2c.buildImage (_utils.library.mkDebugOCI ep o);
in {
  cardano-node = buildDebugImage entrypoints.cardano-node {
    name = "docker.infra.aws.iohkdev.io/cardano-node";
    maxLayers = 25;
    layers = [
      (n2c.buildLayer {deps = entrypoints.cardano-node.runtimeInputs;})
      (n2c.buildLayer {deps = [packages.cardano-node];})
    ];
    contents = [nixpkgs.bashInteractive nixpkgs.iana-etc];
    config.Cmd = [
      "${entrypoints.cardano-node}/bin/entrypoint"
    ];
  };
  cardano-db-sync = buildDebugImage entrypoints.cardano-db-sync {
    name = "docker.infra.aws.iohkdev.io/cardano-db-sync";
    maxLayers = 25;
    layers = [
      (n2c.buildLayer {deps = entrypoints.cardano-db-sync.runtimeInputs;})
      (n2c.buildLayer {deps = [packages.cardano-db-sync];})
    ];
    contents = [nixpkgs.bashInteractive nixpkgs.iana-etc];
    config.Cmd = [
      "${entrypoints.cardano-db-sync}/bin/entrypoint"
    ];
  };
  cardano-wallet = buildDebugImage entrypoints.cardano-wallet {
    name = "docker.infra.aws.iohkdev.io/cardano-wallet";
    maxLayers = 25;
    layers = [
      (n2c.buildLayer {deps = entrypoints.cardano-wallet.runtimeInputs;})
      (n2c.buildLayer {deps = [packages.cardano-wallet];})
    ];
    contents = [nixpkgs.bashInteractive nixpkgs.iana-etc healthChecks.cardano-wallet-network-sync];
    config.Cmd = [
      "${entrypoints.cardano-wallet}/bin/entrypoint"
    ];
  };
  cardano-submit-api = buildDebugImage entrypoints.cardano-submit-api {
    name = "docker.infra.aws.iohkdev.io/cardano-submit-api";
    maxLayers = 25;
    layers = [
      (n2c.buildLayer {deps = entrypoints.cardano-submit-api.runtimeInputs;})
      (n2c.buildLayer {deps = [packages.cardano-submit-api];})
    ];
    contents = [nixpkgs.bashInteractive nixpkgs.iana-etc healthChecks.cardano-submit-api-network-sync];
    config.Cmd = [
      "${entrypoints.cardano-submit-api}/bin/entrypoint"
    ];
  };
}
