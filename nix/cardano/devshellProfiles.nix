{
  inputs,
  cell,
}: let
  inherit (inputs) nixpkgs;
  inherit (cell) packages;
in {
  default = _: {
    commands = [
      {
        package = nixpkgs.b2sum;
        category = "cardano";
      }
      {
        package = nixpkgs.xxd;
        name = "xxd";
        category = "cardano";
      }
      {
        package = nixpkgs.haskellPackages.cbor-tool;
        category = "cardano";
      }
      {
        package = packages.bech32;
        name = "bech32";
        category = "cardano";
      }
      {
        package = packages.cardano-wallet;
        category = "cardano";
      }
      {
        package = packages.cardano-address;
        name = "cardano-address";
        category = "cardano";
      }
      {
        package = packages.cardano-cli;
        name = "cardano-cli";
        category = "cardano";
      }
      {
        package = packages.cardano-ping;
        name = "cardano-ping";
        category = "cardano";
      }
      {
        package = packages.cardano-node;
        name = "cardano-node";
        category = "cardano";
      }
    ];
  };
}
