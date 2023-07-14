{
  inputs,
  cell,
}: let
  inherit (inputs.nixpkgs) lib;

  mkEnv = {
    name,
    nodeClass ? "qa",
    scaling ? 3,
    ...
  }@extraConfig: lib.flip lib.recursiveUpdate ({
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

    # Memory for node needs to be adjusted for chain size.
    # Since memory exhausted clients can be CPU under-utilized,
    # scale the CPU resource to match memory utilization %.
    #
    # For the qa clients, this is a factor of CPU(Mhz) ~= 0.6X RAM(MB).
    (mkEnv {
      name = "preprod";
      nodeCpuMhz = 2000;
      nodeMemoryMB = 3 * 1024;
    })
    (mkEnv {
      name = "preview";
      nodeCpuMhz = 2000;
      nodeMemoryMB = 3 * 1024;
    })
    (mkEnv {
      name = "private";
      nodeCpuMhz = 600;
      nodeMemoryMB = 1 * 1024;
    })
    (mkEnv {
      name = "sanchonet";
      nodeCpuMhz = 1200;
      nodeMemoryMB = 2 * 1024;
    })
    (mkEnv {
      name = "shelley-qa";
      nodeCpuMhz = 1200;
      nodeMemoryMB = 2 * 1024;
    })
  ];
}
