{
  inputs,
  cell,
}: let
  inherit (inputs.nixpkgs) lib;

  mkEnv = {
    name,
    nodeClass ? "qa",
    scaling ? 3,
    extraConfig ? {},
    ...
  }: lib.flip lib.recursiveUpdate ({
    ${name} = {
      inherit nodeClass scaling;
      namespace = name;
      domain = "${name}.${baseDomain}";
    } // extraConfig;
  });

  # Metadata
  # -----------------------------------------------------------------------
  baseDomain = "world.dev.cardano.org";

in {
  # App Components
  # -----------------------------------------------------------------------
  envs = lib.pipe {} [
    (mkEnv {
      name = "infra";
      nodeClass = "infra";
    })
    (mkEnv {
      name = "mainnet";
    })
    (mkEnv {
      name = "perf";
      nodeClass = "perf";
    })
    (mkEnv {
      name = "preprod";
    })
    (mkEnv {
      name = "preview";
    })
    (mkEnv {
      name = "private";
    })
    (mkEnv {
      name = "sanchonet";
    })
    (mkEnv {
      name = "shelley-qa";
      extraConfig.nodeMemoryMB = 2 * 1024;
    })
  ];
}
