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

      github.ci = {
        enable = config.actionRun.facts != {};
        repository = "input-output-hk/cardano-world";
        revision = config.preset.github.lib.readRevision inputs.cells.cloud.library.actionCiInputName null;
      };
    };

    command.text = config.preset.github.status.lib.reportBulk {
      bulk.text = ''
        nix eval .#hydraJobs --json \
          --apply 'jobs: __attrNames (removeAttrs jobs [ "required" ])' \
        | nix-systems -i
      '';
      each.text = ''nix build -L .#hydraJobs."$1".required'';
      skippedDescription = lib.escapeShellArg "No nix builder available for this system";
    };

    memory = 1024 * 6;
    nomad.resources.cpu = 10000;
  };
}
