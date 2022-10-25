{
  inputs,
  cell,
}: let
  inherit (inputs) nixpkgs;
  inherit (inputs.std) n2c;
  inherit (inputs.bitte-cells) _utils;
  inherit (cell) entrypoints;
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
