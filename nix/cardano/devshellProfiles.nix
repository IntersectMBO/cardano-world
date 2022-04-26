{
  inputs,
  cell,
}: let
  inherit (inputs) nixpkgs;
  inherit (nixpkgs) lib;
  inherit (cell) packages;
  inherit (packages.project.pkgs) haskell-nix;
  inherit (packages.project.args) compiler-nix-name;
  inherit (packages.project) index-state;

in {
  default = _: {
    imports = [
      packages.project.devshell
    ];
    commands = [
      {
        package = nixpkgs.b2sum;
        category = "cardano";
      }
      {
        package = nixpkgs.xxd;
        category = "cardano";
        name = "xxd";
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
        package = haskell-nix.tool compiler-nix-name "hlint" {
          version = "3.2.7";
          inherit index-state;
        };
        name = "hlint";
        category = "development";
      }
      {
        package = haskell-nix.tool compiler-nix-name "ghcid" {
          version = "0.8.7";
          inherit index-state;
        };
        name = "ghcid";
        category = "development";
      }
      {
        package = haskell-nix.tool compiler-nix-name "haskell-language-server" {
          version = "1.6.1.1";
          inherit index-state;
        };
        name = "haskell-language-server";
        category = "development";
      }
      {
        package = haskell-nix.tool compiler-nix-name "stylish-haskell" {
          version = "0.13.0.0";
          inherit index-state;
        };
        name = "stylish-haskell";
        category = "development";
      }
      {
        package = inputs.cells.automation.jobs.regenerate-nix;
        category = "nix-build";
      }
    ];
  };
}
