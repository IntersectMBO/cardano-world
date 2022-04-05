{
  inputs,
  cell,
}: let
  inherit (inputs) nixpkgs;
  n2c = inputs.n2c.packages.nix2container;
in {
}
