# This creates the Haskell package set.
# https://input-output-hk.github.io/haskell.nix/user-guide/projects/
haskell-nix: src:
haskell-nix.cabalProject' {
  name = "ogmios";
  src = haskell-nix.haskellLib.cleanSourceWith {
    name = "ogmios-src";
    inherit src;
    subDir = "server";
    filter = path: type:
      builtins.all (x: x) [
        (baseNameOf path != "package.yaml")
      ];
  };
  compiler-nix-name = "ghc8107";
  modules = [
    {
      doHaddock = false;
      doCheck = false;
    }
  ];
}
