{
  inputs,
  cell,
}: let
  inherit (inputs) nixpkgs;
in {
  mdbook =
    nixpkgs.runCommand "mdbook-build" {
      nativeBuildInputs = [nixpkgs.mdbook];
    } ''
      mdbook build --dest-dir "$out" ${inputs.self}
    '';
}
