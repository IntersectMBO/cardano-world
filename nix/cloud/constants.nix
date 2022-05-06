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
  };
  vasil-qa = {
    namespace = "vasil-qa";
    datacenters = ["eu-central-1" "eu-west-1" "us-east-2"];
    domain = "vasil-qa.${baseDomain}";
    nodeClass = "qa";
    scaling = 4;
  };
}
