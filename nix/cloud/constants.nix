{
  inputs,
  cell,
}: let
  # Metadata
  # -----------------------------------------------------------------------
  baseDomain = "world.dev.cardano.org";
in {
  # App Components
  # -----------------------------------------------------------------------
  envs = {
    infra = {
      namespace = "infra";
      domain = "infra.${baseDomain}";
      nodeClass = "infra";
      scaling = 3;
    };
    mainnet = {
      namespace = "mainnet";
      domain = "mainnet.${baseDomain}";
      nodeClass = "qa";
      scaling = 3;
    };
    shelley-qa = {
      namespace = "shelley-qa";
      domain = "shelley-qa.${baseDomain}";
      nodeClass = "qa";
      scaling = 3;
    };
    vasil-dev = {
      namespace = "vasil-dev";
      domain = "vasil-dev.${baseDomain}";
      nodeClass = "qa";
      scaling = 3;
    };
    preprod = {
      namespace = "preprod";
      domain = "preprod.${baseDomain}";
      nodeClass = "qa";
      scaling = 3;
    };
    preview = {
      namespace = "preview";
      domain = "preview.${baseDomain}";
      nodeClass = "qa";
      scaling = 3;
    };
    pv8 = {
      namespace = "pv8";
      domain = "pv8.${baseDomain}";
      nodeClass = "qa";
      scaling = 3;
    };
    private = {
      namespace = "private";
      domain = "private.${baseDomain}";
      nodeClass = "qa";
      scaling = 3;
    };
    perf = {
      namespace = "perf";
      domain = "perf.${baseDomain}";
      nodeClass = "qa";
      scaling = 3;
    };
  };
}
