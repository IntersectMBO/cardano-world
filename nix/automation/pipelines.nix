{
  cell,
  inputs,
}: {
  "cardano-world/ci" = {
    config,
    lib,
    ...
  }: {
    preset = {
      nix.enable = true;
      github-ci = {
        enable = config.action.facts != {};
        repo = "input-output-hk/cardano-world";
        sha = config.preset.github-ci.lib.getRevision "GitHub event" "HEAD";
        clone = false;
      };
    };

    command.text = let
      flakeUrl = lib.escapeShellArg (
        with config.preset.github-ci;
          if enable
          then "github:${repo}/${sha}"
          else "."
      );
    in ''
      system=$(nix eval --raw --impure --expr __currentSystem)
      nix build -L ${flakeUrl}#"$system".automation.hydraJobs.required
    '';

    memory = 1024 * 6;
    nomad.resources.cpu = 10000;
  };
}
