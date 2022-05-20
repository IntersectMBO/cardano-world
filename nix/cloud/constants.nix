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
    vasil-qa = {
      namespace = "vasil-qa";
      domain = "vasil-qa.${baseDomain}";
      nodeClass = "qa";
      scaling = 3;
    };
  };
}
