{
  inputs,
  cell,
}: let
  inherit (inputs) nixpkgs;
  inherit (cell) packages;
  inherit (inputs.bitte-cells._writers.library) writeShellApplication;
in {
  public-documentation-serve = writeShellApplication {
    runtimeInputs = [nixpkgs.darkhttpd];
    name = "entrypoint";
    text = ''
      exec darkhttpd "''${CONFIG_HTML_ROOT:-${packages.mdbook}}" --port "''${NOMAD_PORT_http:-8080}"
    '';
  };
}
