{ inputs
, cell
,
}:
let
  inherit (inputs) nixpkgs cells;
  inherit (nixpkgs) lib;
  inherit (nixpkgs.stdenv) hostPlatform;
  inherit (cell) devshells;
  inherit (cell.jobs) mkHydraRequiredJob;
  jobs = {
    devshells =
      if (nixpkgs.stdenv.hostPlatform.isLinux) then
        devshells else builtins.removeAttrs devshells [ "ops" ];
  };
  nonRequiredPaths = map lib.hasPrefix [ ];
  required = mkHydraRequiredJob nonRequiredPaths jobs;
in
jobs // {
  inherit required;
}
