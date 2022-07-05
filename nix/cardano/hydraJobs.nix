{ inputs
, cell
,
}:
let
  inherit (inputs) nixpkgs cells;
  inherit (nixpkgs) lib;
  inherit (nixpkgs.stdenv) hostPlatform;
  inherit (cells.automation.jobs) mkHydraRequiredJob;
  inherit (cell.packages) project;
  jobs = {
    linux = lib.optionalAttrs hostPlatform.isLinux {
      x86 = lib.optionalAttrs hostPlatform.isx86_64 {
        native = {
          inherit (project) checks exes benchmarks;
          profiled = lib.genAttrs [ "cardano-node" "tx-generator" "locli" ] (n:
            project.exes.${n}.passthru.profiled
          );
          asserted = lib.genAttrs [ "cardano-node" ] (n:
            project.exes.${n}.passthru.asserted
          );
          internal = {
            roots.project = project.roots;
            plan-nix.project = project.plan-nix;
          };
        };
        musl =
          let muslProject = project.projectCross.musl64;
          in
          {
            cardano-node-linux = muslProject.release;
            internal.roots.project = muslProject.roots;
          };
        windows =
          let windowsProject = project.projectCross.mingwW64;
          in
          {
            inherit (windowsProject) checks exes benchmarks;
            cardano-node-win64 = windowsProject.release;
            internal.roots.project = windowsProject.roots;
          };
      };
      arm = lib.optionalAttrs hostPlatform.isAarch64 {
        inherit (project) exes;
        internal.roots.project = project.roots;
      };
    };
    macos = lib.optionalAttrs hostPlatform.isMacOS {
      x86 = lib.optionalAttrs hostPlatform.isx86_64 {
        inherit (project) checks exes benchmarks;
        internal = {
          roots.project = project.roots;
          plan-nix.project = project.plan-nix;
        };
      };
      arm = lib.optionalAttrs hostPlatform.isAarch64 {
        inherit (project) checks exes benchmarks;
        internal = {
          roots.project = project.roots;
          plan-nix.project = project.plan-nix;
        };
      };
    };
  };
  nonRequiredPaths = map lib.hasPrefix [ ];
  required = mkHydraRequiredJob nonRequiredPaths jobs;
in
jobs // {
  inherit required;
}
