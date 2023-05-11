{
  inputs,
  cell,
}: let
  inherit (inputs) nixpkgs iohk-nix;
  inherit (nixpkgs) jq writeText runCommandNoCC lib;
  inherit (cardanoLib) mkConfigHtml;
  cardanoLib = import "${iohk-nix}/cardano-lib/default.nix" {inherit (nixpkgs) lib writeText runCommand jq;};

in {
  generateStaticHTMLConfigs = environments: mkConfigHtml environments;
}
