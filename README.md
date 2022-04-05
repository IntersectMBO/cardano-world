# Cardano

Cardano is a project that declares cardano environments leveraging the
[`bitte`][bitte] stack & cloud scheduler, beyond kubernetes.

## Repo Layout Brief

The application source code _could_ live in [`./src`][app].

The application is _parasitically_ operationalized in [`./nix`][ops].

[`./nix`][ops] provides the following **Cells**:

- Metal (AWS, The Hosts' OS, Datacenter Scheduler, Service Discovery, Secrets Vault, Monitoring, DNS & Ingress Routing)
- Cloud (Schedulable Jobs & Cluster Hydration Profiles)
- Repoautomation (Runnable helpers for daily or sporadic tasks)

This repository defines the following **Organelles**:

- **Installables**:
  - Packages (flake output: `packages`)
- **Runnables**:
  - Just Tasks ["repoautomation"] (flake output: `justTasks`)
- **Functions**:
  - OCI Images ["docker"] (flake output: `oci-images`)
  - Bitte Profile (flake output: `bitteProfile`)
  - Nomad Schedulable Environments (flake output: `nomadEnvs`)
  - Hydration Profiles (`hydrationProfiles`)

You'll find further information about this nomenclature in the
[Standard Readme][std-readme].

## Development Environments

You can find all information about entering a fully pre-configured &
reproducible development environment in [`./devshell`][devshell].

## Application Documentation

You can find all information about the application itself in [`./src`][app].

## Operatations

You can find all information about the operations of the application [`./nix`][ops].

---

[app]: ./src
[bitte]: https://github.com/input-output-hk/bitte
[devshell]: ./devshell
[ops]: ./nix
[std-readme]: https://github.com/divnix/std#readme
