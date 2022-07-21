{
  cell,
  inputs,
}: {
  "cardano-world/ci" = {
    task = "cardano-world/ci";
    io = ''
      _lib: github: {
        #repo: "input-output-hk/cardano-world"
        push: {}
        pull_request: {}
      }
    '';
  };
}
