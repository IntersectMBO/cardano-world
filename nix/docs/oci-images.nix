{
  inputs,
  cell,
}: let
  inherit (inputs) nixpkgs;
  inherit (inputs.bitte-cells) _utils;
  inherit (cell) entrypoints;
  n2c = inputs.n2c.packages.nix2container;
in {
  public-documentation = n2c.buildImage {
    name = "registry.ci.iog.io/cardano-public-documentation";
    maxLayers = 25;
    layers = [(n2c.buildLayer {deps = [nixpkgs.mdbook];})];
    contents = [nixpkgs.bashInteractive];
    config.Cmd = [
      "${entrypoints.public-documentation-serve}/bin/entrypoint"
    ];
  };
}
