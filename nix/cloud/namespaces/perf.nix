{
  inputs,
  cell,
}: let
  inherit (inputs) data-merge;
  inherit (inputs.cells) cardano;
  inherit (cell) constants;

in {

  # Insert job defns here.
  # See other nix/cloud/namespaces/*.nix files for examples.
}
