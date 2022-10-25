{
  inputs,
  cell,
}: let
  inherit (inputs) nixpkgs;
  inherit (inputs.std) std;
in {
  treefmt = std.nixago.treefmt {
    configData = {
      formatter.prettier = {
        excludes = [
          "*-genesis.json"
          "*.enc.json"
          "*.enc.yaml"
        ];
      };
      formatter.haskell = {
        command = "ormolu";
        options = [
          "--ghc-opt"
          "-XBangPatterns"
          "--ghc-opt"
          "-XPatternSynonyms"
          "--ghc-opt"
          "-XTypeApplications"
          "--mode"
          "inplace"
          "--check-idempotence"
        ];
        includes = ["*.hs"];
      };
    };
    packages = [nixpkgs.ormolu];
  };
  editorconfig = std.nixago.editorconfig {
    configData = {
      "*.hs" = {
        indent_style = "space";
        indent_size = 2;
        trim_trailing_whitespace = "true";
        insert_final_newline = "true";
        charset = "utf-8";
        end_of_line = "lf";
      };

      "Makefile" = {
        indent_style = "tab";
      };
      "*-genesis.json" = {
        end_of_line = "unset";
        insert_final_newline = "unset";
        trim_trailing_whitespace = "unset";
        charset = "unset";
        indent_style = "unset";
        indent_size = "unset";
      };
      "nix/metal/encrypted/*" = {
        end_of_line = "unset";
        insert_final_newline = "unset";
        trim_trailing_whitespace = "unset";
        charset = "unset";
        indent_style = "unset";
        indent_size = "unset";
      };
      "{secrets/**,**.enc,**.enc.*}" = {
        end_of_line = "unset";
        insert_final_newline = "unset";
        trim_trailing_whitespace = "unset";
        charset = "unset";
        indent_style = "unset";
        indent_size = "unset";
      };
    };
  };
  mdbook = std.nixago.mdbook {
    packages = [nixpkgs.mdbook-mermaid];
    configData = {
      book.autors = ["The Cardano Authors"];
      book.title = "The Cardano Operations Book";
      output.html = {
        git-repository-url = "https://github.com/input-output-hk/cardano-world";
        git-repository-icon = "fa-github";
        edit-url-template = "https://github.com/input-output-hk/cardano-world/edit/master/src/{path}";
      };
    };
  };
}
