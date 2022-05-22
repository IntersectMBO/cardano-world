# Packaging Principles

All Cardano software is packaged according to the following principles.

- Reproducibility
- Uncompromising GitOps
- The 4 Layers of Packaging

## Reproducibility

All Cardano software published via this repository is built in a _reproducible_
manner. That means, given a specific checkout of this repository, anyone will
be able with time and knowledge to reproduce byte-equivalent artifacts compared
to the ones that we publish as releases.

This reproducibility is such that it percolates the entire build stack and
up to the most fundamental bootstrapping of the compiler toolchain.

Reproducability produces high-integrity build artifacts and is the only route
to sensible software supply chain auditing. &rarr;
We build Cardano with the same rigor as we write it.

#### Additional Resources

- If this is new to you, see: [https://reproducible-builds.org/][reproducible]

- And for even more mind-blow, see: [https://bootstrappable.org/][bootstrappable]

- Make sure to also read up the legendary [Ken Thompson's "Reflections on Trusting Trust"][trusting-trust].

## Uncompromising GitOps

Building on the previous principle, all our build instructions are _code_. That means
with a simple checkout, a single system dependency on [`nix`][nix] and simple
`nix` commands, you can do one or all of the following things (and more):

- Build a binary (and also publish it)
- Build an entrypoint
- Build an OCI-Image (and also publish it)
- Build the scheduler spec (and also deploy it)

## The 4 Layer of Packaging

Based on the previous principles, to get a runnable shipped, we look at 4 layers of packaging:

- The binary packaging ([`nix/cardano/packages/default.nix`][packages])
- The entrypoint packaging ([`nix/cardano/entrypoints.nix`][entrypoints])
- The OCI image packaging ([`nix/cardano/oci-images.nix`][images])
- The scheduler packaging ([`nix/cardano/nomadJob/default.nix`][jobs])

You can consume any of those artifacts according to your deployment scenario, but usually
using our published OCI-images ("docker") might be a good start.

---

[nix]: https://nixos.org
[trusting-trust]: http://users.ece.cmu.edu/~ganger/712.fall02/papers/p761-thompson.pdf
[packages]: https://github.com/input-output-hk/cardano-world/tree/master/nix/cardano/packages/default.nix
[entrypoints]: https://github.com/input-output-hk/cardano-world/tree/master/nix/cardano/entrypoints.nix
[images]: https://github.com/input-output-hk/cardano-world/tree/master/nix/cardano/oci-images.nix
[jobs]: https://github.com/input-output-hk/cardano-world/tree/master/nix/cardano/nomadJob/default.nix
[bootstrappable]: https://bootstrappable.org/
[reproducible]: https://reproducible-builds.org/
