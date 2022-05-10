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
    vasil-qa = {
      namespace = "vasil-qa";
      domain = "vasil-qa.${baseDomain}";
      nodeClass = "qa";
      scaling = 3;
    };
  };
}
