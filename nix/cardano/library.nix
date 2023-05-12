{
  inputs,
  cell,
}: let
  inherit (inputs) nixpkgs iohk-nix;
  inherit (nixpkgs) lib runCommand;
  inherit (cardanoLib) mkConfigHtml;
  cardanoLib = import "${iohk-nix}/cardano-lib/default.nix" {inherit (nixpkgs) lib writeText runCommand jq;};

in rec {
  # Use the iohk-nix mkConfigHtml attr and transform the output to what mdbook expects
  generateStaticHTMLConfigs = environments: let
    cardano-deployment = mkConfigHtml environments;
  in runCommand "cardano-html" {} ''
    mkdir "$out"
    cp "${cardano-deployment}/index.html" "$out/"
    cp "${cardano-deployment}/rest-config.json" "$out/"
    ENVS=(${nixpkgs.lib.escapeShellArgs (builtins.attrNames environments)})
    for ENV in "''${ENVS[@]}"; do
      # Migrate each env from a flat dir to an ENV subdir
      mkdir -p "$out/config/$ENV"
      for i in $(find ${cardano-deployment} -type f -name "$ENV-*" -printf "%f\n"); do
        cp -v "${cardano-deployment}/$i" "$out/config/$ENV/''${i#"$ENV-"}"
      done

      # Adjust genesis file and config refs
      sed -i "s|\"$ENV-|\"|g" "$out/config/$ENV/config.json"
      sed -i "s|\"$ENV-|\"|g" "$out/config/$ENV/db-sync-config.json"

      # Adjust index.html file refs
      sed -i "s|$ENV-|config/$ENV/|g" "$out/index.html"
    done

  '';

  # Copy the environment configs from iohk-nix into our jobs at runtime
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
