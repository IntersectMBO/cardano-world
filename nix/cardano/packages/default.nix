{
  inputs,
  cell,
}: let
  inherit (inputs) nixpkgs;
  inherit (inputs) std;
in {
  inherit (inputs.cardano-node.packages) cardano-node cardano-cli cardano-submit-api;
  #cardano-node = nixpkgs.hello;
}
