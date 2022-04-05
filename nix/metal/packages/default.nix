{
  inputs,
  cell,
}: let
  inherit (inputs) nixpkgs;
in {
  traefik = nixpkgs.callPackage ./traefik.nix {};
}
