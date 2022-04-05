{
  inputs,
  cell,
}: let
  inherit (inputs) data-merge;
  inherit (inputs.bitte-cells) cardano patroni vector;
  inherit (cell) constants;

  mkComponents = args: {
  };

  # Components per environment
  # -----------------------------------------------------------------------
  testnet-prod = mkComponents constants.envs.prod;
  testnet-dev = mkComponents constants.envs.dev;
  # testnet-staging = mkComponents constants.envs.staging;
in
  with data-merge; {
  }
