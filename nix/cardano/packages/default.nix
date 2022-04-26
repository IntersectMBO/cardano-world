{
  inputs,
  cell,
}: let
  inherit (inputs) std nixpkgs iohkNix;
  inherit (nixpkgs) lib;
  inherit (cell) config-data;

  inherit (import inputs.nixpkgs-haskell {
    inherit (nixpkgs) system;
    inherit (inputs.haskellNix) config;
    overlays = with iohkNix.overlays; [
      inputs.haskellNix.overlay
      haskell-nix-extra
      crypto
      (final: prev: {
        haskellBuildUtils = prev.haskellBuildUtils.override {
          inherit compiler-nix-name index-state;
        };
      })
    ];
  }) haskell-nix;

  inherit (haskell-nix) haskellLib;

  project = with haskellLib.projectOverlays;
   (import ./haskell.nix {
    inherit haskell-nix;
    inherit (inputs) byron-chain;
    src = inputs.self;
  }).appendOverlays [
    devshell
    projectComponents
    (final: prev: {
      release = nixpkgs.callPackage ./binary-release.nix {
        inherit (final.pkgs) stdenv;
        exes = lib.attrValues final.exes ++ [
          final.hsPkgs.bech32.components.exes.bech32
        ];
        inherit (final.exes.cardano-node.identifier) version;
        inherit config-data;
      };
      profiled = final.appendModule {
        modules = [{
          enableLibraryProfiling = true;
          packages.cardano-node.components.exes.cardano-node.enableProfiling = true;
          packages.tx-generator.components.exes.tx-generator.enableProfiling = true;
          packages.locli.components.exes.locli.enableProfiling = true;
        }];
      };
      asserted = final.appendModule {
        modules = [{
          packages = lib.genAttrs [
            "ouroboros-consensus"
            "ouroboros-consensus-cardano"
            "ouroboros-consensus-byron"
            "ouroboros-consensus-shelley"
            "ouroboros-consensus-mock"
            "ouroboros-network"
            "network-mux"
            "io-classes"
            "strict-stm"
          ]
            (name: { flags.asserts = true; });
        }];
      };
      eventlogged = final.appendModule
        {
          modules = [{
            packages = lib.genAttrs [ "cardano-node" ]
              (name: { configureFlags = [ "--ghc-option=-eventlog" ]; });
          }];
        };
      hsPkgs = lib.mapAttrsRecursiveCond (v: !(lib.isDerivation v))
        (path: value:
          if (lib.isAttrs value) then
            lib.recursiveUpdate value
              {
                passthru = {
                  profiled = lib.getAttrFromPath path final.profiled.hsPkgs;
                  asserted = lib.getAttrFromPath path final.asserted.hsPkgs;
                  eventlogged = lib.getAttrFromPath path final.eventlogged.hsPkgs;
                };
              } else value) prev.hsPkgs;
    })
  ];

  inherit (project.args) compiler-nix-name;
  inherit (project) index-state;
  inherit (project.hsPkgs.bech32.components.exes) bech32;

in project.exesFrom ./packages-exes.nix // {
  inherit project bech32;
}
