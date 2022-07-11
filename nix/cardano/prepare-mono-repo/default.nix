{ inputs
, cell
,
}:
let
  inherit (inputs) nixpkgs cardano-node;
  inherit (inputs.bitte-cells._writers.library) writeShellApplication;
  inherit (nixpkgs) lib;
  inherit (cell.packages) project;
  # https://github.com/NixOS/nixpkgs/issues/179788:
  inherit (nixpkgs) fetchFromGitHub;

  merge-mono-repo =
    let
      ouroboros = fetchFromGitHub {
        owner = "input-output-hk";
        repo = "ouroboros-network";
        rev = project.pkg-set.config.packages.ouroboros-network.src.rev;
        sha256 = lib.fileContents ./ouroboros-network.sha256;
      };
      ouroborosProject = project.appendModule {
        src = lib.mkForce ouroboros;
      };
      ledger = fetchFromGitHub {
        owner = "input-output-hk";
        repo = "cardano-ledger";
        rev = project.pkg-set.config.packages.cardano-ledger-core.src.rev;
        sha256 = lib.fileContents ./cardano-ledger.sha256;
      };
      ledgerProject = project.appendModule {
        src = lib.mkForce ledger;
      };
      ekgforward = fetchFromGitHub {
        owner = "input-output-hk";
        repo = "ekg-forward";
        rev = project.pkg-set.config.packages.ekg-forward.src.rev;
        sha256 = lib.fileContents ./ekg-forward.sha256;
      };
      packagePaths = project: lib.concatStringsSep " " (lib.mapAttrsToList (_: p: let subdir = lib.removePrefix "/" p.src.origSubDir; in "--path-rename ${subdir}:src/${subdir} --path ${subdir}") project.packages);
      cabalProject = builtins.toFile "cabal.project" (import ./cabal.project.nix {
        inherit lib;
        inherit (project) index-state;
        packages = lib.attrValues (project.packages // ouroborosProject.packages // ledgerProject.packages // {
          ekg-forward = {
            identifier.name = "ekg-forward";
            src.origSubDir = "/ekg-forward";
          };
        });
        cardano-base-src = project.pkg-set.config.packages.cardano-binary.src;
        plutus-src = project.pkg-set.config.packages.plutus-core.src;
      });
    in
    writeShellApplication {
        description = "Create/Replace the mono-repo branch of current git repo";
        name = "merge-mono-repo";
        runtimeInputs = with nixpkgs; [ git-filter-repo git nix ];
        text = ''
      # go to project root directory:
      while [[ $PWD != / && ! -e ".git" ]]; do
        cd ..
      done
      world="$(pwd)"

      cd "$(mktemp -d)"

      git clone git@github.com:input-output-hk/cardano-node node
      git clone git@github.com:input-output-hk/ouroboros-network ouroboros
      git clone git@github.com:input-output-hk/cardano-ledger ledger
      git clone git@github.com:input-output-hk/ekg-forward ekgforward

      cd ouroboros
      ouroboros_repo="$(pwd)"
      git reset --hard ${ouroboros.rev}
      git-filter-repo --force --path-rename docs:docs/network --path docs ${packagePaths ouroborosProject}
      git filter-repo --force --path-glob '*.nix' --invert-paths

      cd ..

      cd ledger
      ledger_repo="$(pwd)"
      git reset --hard ${ledger.rev}
      git-filter-repo --force --path-rename doc:docs/ledger --path-rename docs:docs/ledger  --path docs --path doc \
       --path-rename eras/alonzo/formal-spec:docs/ledger/eras/alonzo/formal-spec --path eras/alonzo/formal-spec \
       --path-rename eras/babbage/formal-spec:docs/ledger/eras/babbage/formal-spec --path eras/babbage/formal-spec \
       --path-rename eras/byron/cddl-spec:docs/ledger/eras/byron/cddl-spec --path eras/byron/cddl-spec \
       --path-rename eras/byron/chain/formal-spec:docs/ledger/eras/byron/chain/formal-spec --path eras/byron/chain/formal-spec \
       --path-rename eras/byron/ledger/formal-spec:docs/ledger/eras/byron/ledger/formal-spec --path eras/byron/ledger/formal-spec \
       --path-rename eras/shelley-ma/formal-spec:docs/ledger/eras/shelley-ma/formal-spec --path eras/shelley-ma/formal-spec \
       --path-rename eras/shelley/design-spec:docs/ledger/eras/shelley/design-spec --path eras/shelley/design-spec \
       --path-rename eras/shelley/formal-spec:docs/ledger/shelley/eras/formal-spec --path eras/shelley/formal-spec \
       ${packagePaths ledgerProject}
      git filter-repo --force --path-glob '*.nix' --path-glob '*/.ghcid' --path-glob '*/.gitignore' --invert-paths

      cd ..

      cd ekgforward
      ekgforward_repo="$(pwd)"
      git reset --hard ${ekgforward.rev}
      git-filter-repo --force --path demo --path src --path test --path ekg-forward.cabal --path README.md --path CHANGELOG.md --path LICENSE
      git-filter-repo --to-subdirectory-filter src/ekg-forward

      cd ..

      cd node
      node_repo="$(pwd)"
      git reset --hard ${cardano-node.rev}
      # cherry-pick faucet
      #git cherry-pick ca31cab600bf24f3732903e09c9c2a0d4423a2ac
      git-filter-repo --force --path cabal.project --path-rename doc:docs/node --path doc --path scripts ${packagePaths project}

      cd "$world"
      git branch -D mono-repo || true
      git checkout -b mono-repo

      git remote rm ledger || true
      git remote add -f ledger "$ledger_repo"
      git merge ledger/master --allow-unrelated-histories --no-ff

      git remote rm ouroboros || true
      git remote add -f ouroboros "$ouroboros_repo"
      git merge ouroboros/master --allow-unrelated-histories --no-ff

      git remote rm ekgforward || true
      git remote add -f ekgforward "$ekgforward_repo"
      git merge ekgforward/master --allow-unrelated-histories --no-ff

      git remote rm node || true
      git remote add -f node "$node_repo"
      git merge node/master --allow-unrelated-histories --no-ff

      cp ${cabalProject} cabal.project
      git apply ${./cabal.diff}
      git add .
      git commit -a -m "Adapt cabal build after merge into mono repo"

      git apply -3 nix/cardano/prepare-mono-repo/remove-prepare-mono-repo.diff
      nix flake lock --update-input cardano-node
      git commit -a -m "Adapt nix build after merge into mono repo"
    '';
  };
in
{
  inherit merge-mono-repo;
}
