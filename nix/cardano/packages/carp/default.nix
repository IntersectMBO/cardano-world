{inputs}: let
  inherit (inputs) rust-overlay naersk;
  nixpkgs = inputs.nixpkgs.appendOverlays [rust-overlay];
  inherit (nixpkgs) lib fetchFromGitHub;
  readTOML = file: builtins.fromTOML (builtins.readFile file);
  carp = nixpkgs.fetchFromGitHub {
    owner = "dcSpark";
    repo = "carp";
    rev = "740fa51b280330bd392f5b2dde1c25afe1ce2e7f";
    sha256 = "sha256-iuT7fffftMkjcTHayCcne1mNqkcxzKnEYl62n65V7Z4=";
  };
  workspaceCargo = readTOML (carp + "/indexer//Cargo.toml");

  rust = let
    _rust = nixpkgs.rust-bin.stable.latest.default.override {
      extensions = [
        "rust-src"
        "rust-analysis"
        "rls-preview"
        "rustfmt-preview"
        "clippy-preview"
      ];
    };
  in
    nixpkgs.buildEnv {
      name = _rust.name;
      inherit (_rust) meta;
      buildInputs = [nixpkgs.makeWrapper];
      paths = [_rust];
      pathsToLink = ["/" "/bin"];
      # XXX: This is needed because cargo and clippy commands need to
      # also be aware of other binaries in order to work properly.
      # https://github.com/cachix/pre-commit-hooks.nix/issues/126
      postBuild = ''
        for i in $out/bin/*; do
          wrapProgram "$i" --prefix PATH : "$out/bin"
        done
      '';
    };

  naersk-lib = naersk.lib.override {
    cargo = rust;
    rustc = rust;
  };

  mkPackage = name: let
    pkgCargo = readTOML carp + "${name}/Cargo.toml";
  in
    naersk-lib.buildPackage {
      inherit (pkgCargo.package) name version;

      root = carp;

      PROTOC = "${nixpkgs.protobuf}/bin/protoc";
      PROTOC_INCLUDE = "${nixpkgs.protobuf}/include";

      nativeBuildInputs = with nixpkgs; [
        pkg-config
        protobuf
        rustfmt
      ];

      buildInputs = with nixpkgs; [
        openssl
      ];
    };

  workspace =
    builtins.listToAttrs
    (
      builtins.map
      (name: {
        inherit name;
        value = mkPackage name;
      })
      workspaceCargo.workspace.members
    );
in {
  inherit (workspace) indexer;
}
