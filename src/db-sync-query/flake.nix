{
  inputs.flake-utils.url = "github:numtide/flake-utils";
  outputs = { nixpkgs, flake-utils, ... }:
    flake-utils.lib.eachSystem [ "x86_64-linux" "aarch64-linux" ] (system:
    let
      pkgs = nixpkgs.legacyPackages.${system};
      compilerVersion = "ghc8107";
      haskellPackages = pkgs.haskell.packages."${compilerVersion}";
    in {
      devShells.default = (pkgs.haskellPackages.developPackage {
        root = ./.;
        source-overrides = {
          # named = builtins.fetchTarball
          #   "https://github.com/monadfix/named/archive/e684a00.tar.gz";
        };
        modifier = drv: pkgs.haskell.lib.overrideCabal drv (attrs: {
          buildTools = (attrs.buildTools or []) ++ [
            haskellPackages.cabal-install
            haskellPackages.hpack
            pkgs.libsodium
            pkgs.zlib
          ];

          # testHaskellDepends = attrs.testHaskellDepends ++ [
          #   pkgs.nix
          #   haskellPackages.criterion
          # ];

          # Declare that the header set arguments as according Haskell.lib switches
          enableSharedLibraries = true;

          # configureFlags = pkgs.lib.optional doTracing  "--flags=tracing";

          passthru = {
            nixpkgs = pkgs;
          };
        });
      }).env;

    });

  nixConfig = {
    extra-substituters = [
      # TODO: spongix
      # "https://cache.iog.io"
    ];
    extra-trusted-public-keys = [
      "hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ="
    ];
    # post-build-hook = "./upload-to-cache.sh";
    allow-import-from-derivation = "true";
  };
}
