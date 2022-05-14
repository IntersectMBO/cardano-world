let
let
  pkgs = import nixpkgs {
    inherit system;
    inherit (haskellNix) config;
    overlays = [
      haskellNix.overlay
      iohkNix.overlays.utils
      iohkNix.overlays.crypto
      iohkNix.overlays.haskell-nix-extra
      iohkNix.overlays.cardano-lib
      overlay
    ];
  };
  project = (import ./nix/haskell.nix pkgs.haskell-nix ogmios);
in project.hsPkgs.ogmios.components.exes.ogmios
