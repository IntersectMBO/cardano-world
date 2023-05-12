{
  inputs,
  cell,
}: let
  inherit (inputs) nixpkgs iohk-nix;
  inherit (cardanoLib) mkConfigHtml;
  cardanoLib = import "${iohk-nix}/cardano-lib/default.nix" {inherit (nixpkgs) lib writeText runCommand jq;};

in rec {
  generateStaticHTMLConfigs = environments: mkConfigHtml environments;

  copyEnvsTemplate = environments: let
    envCfgs = generateStaticHTMLConfigs environments;
  in ''
    # DATA_DIR is a runtime entrypoint env var which will contain the cp target
    mkdir -p "$DATA_DIR/config"
    ENVS=(${nixpkgs.lib.escapeShellArgs (builtins.attrNames environments)})
    for ENV in "''${ENVS[@]}"; do
      cp -rv "${envCfgs}/config/$ENV" "$DATA_DIR/config/"
    done
  '';
}
