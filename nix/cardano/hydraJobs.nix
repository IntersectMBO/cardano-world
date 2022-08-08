{ inputs
, cell
,
}:
let
  inherit (inputs) nixpkgs cells;
  inherit (nixpkgs) lib;
  inherit (nixpkgs.stdenv) hostPlatform;
  inherit (cells.automation.jobs) mkHydraRequiredJob;
  inherit (cell.packages) project nodeProject ogmiosProject;
  mergedProject = lib.recursiveUpdate (lib.recursiveUpdate
    (removeAttrs ogmiosProject ["checks" "benchmarks"]) nodeProject) project;
  internal = {
    roots = {
      nodeProject = nodeProject.roots;
      project = project.roots;
      ogmiosProject = ogmiosProject.roots;
    };
    plan-nix = {
      nodeProject = nodeProject.plan-nix;
      project = project.plan-nix;
      ogmiosProject = ogmiosProject.plan-nix;
    };
  };
  jobs = {
    linux = lib.optionalAttrs hostPlatform.isLinux {
      x86 = lib.optionalAttrs hostPlatform.isx86_64 {
        native = {
          inherit (mergedProject) exes checks benchmarks;
          profiled = lib.genAttrs [ "cardano-node" "tx-generator" "locli" "cardano-new-faucet" ] (n:
            mergedProject.exes.${n}.passthru.profiled
          );
          asserted = lib.genAttrs [ "cardano-node" ] (n:
            nodeProject.exes.${n}.passthru.asserted
          );
          inherit internal;
        };
        musl =
          let
            muslProject = nodeProject.projectCross.musl64;
          in
          {
            cardano-node-linux = muslProject.release;
            internal.roots.nodeProject = muslProject.roots;
          };
        windows =
          let windowsProject = nodeProject.projectCross.mingwW64;
          in
          {
            inherit (windowsProject) checks benchmarks;
            cardano-node-win64 = windowsProject.release;
            internal.roots.nodeProject = windowsProject.roots;
          };
      };
      arm = lib.optionalAttrs hostPlatform.isAarch64 {
        inherit (mergedProject) exes;
        inherit internal;
      };
    };
    macos = lib.optionalAttrs hostPlatform.isMacOS {
      x86 = lib.optionalAttrs hostPlatform.isx86_64 {
        inherit (mergedProject) exes checks benchmarks;
        inherit internal;
      };
      arm = lib.optionalAttrs hostPlatform.isAarch64 {
        inherit (mergedProject) exes checks benchmarks;
        inherit internal;
      };
    };
  };
  nonRequiredPaths = map lib.hasPrefix [ ];
  required = mkHydraRequiredJob nonRequiredPaths jobs;
in
jobs // {
  inherit required;
}
