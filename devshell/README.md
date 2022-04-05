# Development Environments

## `direnv`

To quickly enter a development environment every time you enter this repository,
please [install][direnv-install] the great and unmatched [`direnv`][direnv] tool.

## `nix develop -c $SHELL ...`

- Loads temporarily an environment by setting the appropriate environment variables. CTRL+C to exit again.
- You need the latest official [`nix`][nix-install] version `>=2.7`, preferably in multi-user install mode (`nix --version`).
- For your convenience, there is tab completion on `nix develop ./devshell#<TAB>` (albeit a bit lagging).

## The Environments

#### `__default` &rarr; `nix develop ./devshell#__default -c $SHELL` | `direnv allow`

A fully fledged development environment tailored to operational needs.

All secrets to access the cluster resources either via the CLI or through the web interface
are automatically set up. You'll be greeted by a useful message of the day [MOTD].

The only pre-requisit is that you have well-known [`~/.netrc`][netrc] file set up with your personal
github API token.

You can inpect the environment & secrets via `env | grep NOMAD`, `env | grep VAULT`,
`env | grep CONSUL`, `env | grep AWS`, etc.

#### `cardano` &rarr; `nix develop ./devshell#cardano -c $SHELL`

_Not set-up yet._

A fully configured environment to develop or debug the cardano application itself.

---

[direnv]: https://direnv.net/
[direnv-install]: https://direnv.net/docs/installation.html
[netrc]: ./netrc.md
[nix-install]: https://nixos.org/download.html
