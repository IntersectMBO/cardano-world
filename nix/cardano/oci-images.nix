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
    name = "registry.ci.iog.io/cardano-node";
    maxLayers = 25;
    layers = [
      (n2c.buildLayer {deps = entrypoints.cardano-node.runtimeInputs;})
      (n2c.buildLayer {deps = [packages.cardano-node];})
    ];
    contents = [nixpkgs.bashInteractive nixpkgs.iana-etc];
    config.Cmd = [
      "${entrypoints.cardano-node}/bin/entrypoint"
    ];
    config.User = "1000:1000";
  };
  cardano-db-sync = buildDebugImage entrypoints.cardano-db-sync {
    name = "registry.ci.iog.io/cardano-db-sync";
    maxLayers = 25;
    layers = [
      (n2c.buildLayer {deps = entrypoints.cardano-db-sync.runtimeInputs;})
      (n2c.buildLayer {deps = [packages.cardano-db-sync];})
    ];
    contents = [nixpkgs.bashInteractive nixpkgs.iana-etc];
    config.Cmd = [
      "${entrypoints.cardano-db-sync}/bin/entrypoint"
    ];
    config.User = "1000:1000";
  };
  cardano-wallet = buildDebugImage entrypoints.cardano-wallet {
    name = "registry.ci.iog.io/cardano-wallet";
    maxLayers = 25;
    layers = [
      (n2c.buildLayer {deps = entrypoints.cardano-wallet.runtimeInputs;})
      (n2c.buildLayer {deps = [packages.cardano-wallet];})
    ];
    contents = [nixpkgs.bashInteractive nixpkgs.iana-etc healthChecks.cardano-wallet-network-sync];
    config.Cmd = [
      "${entrypoints.cardano-wallet}/bin/entrypoint"
    ];
    config.User = "1000:1000";
  };
  cardano-submit-api = buildDebugImage entrypoints.cardano-submit-api {
    name = "registry.ci.iog.io/cardano-submit-api";
    maxLayers = 25;
    layers = [
      (n2c.buildLayer {deps = entrypoints.cardano-submit-api.runtimeInputs;})
      (n2c.buildLayer {deps = [packages.cardano-submit-api];})
    ];
    contents = [nixpkgs.bashInteractive nixpkgs.iana-etc healthChecks.cardano-submit-api-network-sync];
    config.Cmd = [
      "${entrypoints.cardano-submit-api}/bin/entrypoint"
    ];
    config.User = "1000:1000";
  };
  ogmios = buildDebugImage entrypoints.ogmios {
    name = "registry.ci.iog.io/ogmios";
    maxLayers = 25;
    layers = [
      (n2c.buildLayer {deps = entrypoints.ogmios.runtimeInputs;})
      (n2c.buildLayer {deps = [packages.ogmios];})
    ];
    contents = [nixpkgs.bashInteractive];
    config.Cmd = [
      "${entrypoints.ogmios}/bin/entrypoint"
    ];
    config.User = "1000:1000";
  };
  oura = buildDebugImage entrypoints.oura {
    name = "registry.ci.iog.io/oura";
    maxLayers = 25;
    layers = [
      # (n2c.buildLayer {deps = entrypoints.oura.runtimeInputs;})
      (n2c.buildLayer {deps = [packages.oura];})
    ];
    contents = [nixpkgs.bashInteractive];
    config.Cmd = [
      "${entrypoints.oura}/bin/entrypoint"
    ];
    config.User = "nobody:nogroup";
  };
  scrolls = buildDebugImage entrypoints.scrolls {
    name = "registry.ci.iog.io/scrolls";
    maxLayers = 25;
    layers = [
      # (n2c.buildLayer {deps = entrypoints.scrolls.runtimeInputs;})
      (n2c.buildLayer {deps = [packages.scrolls];})
    ];
    contents = [nixpkgs.bashInteractive];
    config.Cmd = [
      "${entrypoints.scrolls}/bin/entrypoint"
    ];
    config.User = "nobody:nogroup";
  };


}
