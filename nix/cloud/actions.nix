{
  cell,
  inputs,
}: {
  "cardano-world/ci" = {
    task = "cardano-world/ci";
    io = ''
      let github = {
        #input: "${cell.library.actionCiInputName}"
        #repo: "input-output-hk/cardano-world"
      }

      #lib.merge
      #ios: [
        #lib.io.github_push & github,
        #lib.io.github_pr   & github,
      ]
    '';
  };
}
