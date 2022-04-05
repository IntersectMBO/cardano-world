{
  inputs,
  makeScript,
  system,
  ...
}: let
  network = "testnet";
  nixpkgs = inputs.nixpkgs;
  bitte-cells = inputs.bitte-cells.packages.${system};
in
  makeScript {
    entrypoint = ./entrypoint.sh;
    name = "create-wallet";
    help = ./README.md;
    replace = {
      __argWorkbench__ = "./workbench/create-wallet/${network}";
      __argNetwork__ = network;
    };
    searchPaths.bin = [bitte-cells.cardano-address nixpkgs.jq nixpkgs.pwgen];
  }
