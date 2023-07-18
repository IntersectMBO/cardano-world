# Environments

The official Cardano Environment configuration files:

## Throw-away Devnets

Usage: Hard fork testing, Testing new release functionality that needs public involvement

These can be spun up/torn down at any time by release manager for specific tests, such as hard forks.

These are shared with the community for early involvment before a release candidate is mature.

Recieves updates for official releases, release candidates and occasionally custom tags that aren't candidates for release

Upgrade Strategy: Deploy all nodes with every request

Changes Requested by: Release Manager
Approvals Required: SRE Director or SRE Resident Manager

Responsible: SRE
Accountable: SRE Director
Consulted: Release Manager
Informed: Cardano Core Tribe

## Sanchonet Testnet

Usage: Testing Conway era functionality.

The Sanchonet chain will be rolled back with each new node release for testing new features and fixes.

When respun the chain will be restored from slot 259619.  Any Sanchonet chain participants, stakepools,
integrators, etc, will need to clear their chain state to re-sync from that point forward when the chain is respun.

Epoch length of 1 day. Development flags allowed in configuration files.

Upgrade Strategy: Deploy all nodes with every upgrade request

Responsible: IOG SRE
Accountable: SRE Director
Consulted: Core Tech Head of Product
Informed: Cardano Core Tribe

- [Node Config](environments/sanchonet/config.json)
- [DB Sync Config](environments/sanchonet/db-sync-config.json)
- [Submit API Config](environments/sanchonet/submit-api-config.json)
- [Node Topology](environments/sanchonet/topology.json)
- [Byron Genesis](environments/sanchonet/byron-genesis.json)
- [Shelley Genesis](environments/sanchonet/shelley-genesis.json)
- [Alonzo Genesis](environments/sanchonet/alonzo-genesis.json)
- [Conway Genesis](environments/sanchonet/conway-genesis.json)

## Preview Testnet

Usage: Testing release candidates and mainnet releases. Leads mainnet hard forks by at least 4 weeks.

Ideally stays long running. Only if an issue is found after it forks that's breaking should it be respun.

Epoch length of 1 day. Development flags allowed in configuration files.

Upgrade Strategy:

- Release Candidates - 1/3 of nodes
- Official Releases - 2/3 of nodes
- Hard forks - all nodes
- Community requested to only deploy release candidates and official releases

Changes Requested by: Release Squad Lead
Approvals Required: SRE Tribe Lead, Cardano Head of Engineering, Cardano Head of Architecture

Responsible: IOG SRE
Accountable: Head of SRE/Release Squad Lead
Consulted: SPOs
Informed: Cardano Core Tribe, COO, Director of Engineering

- [Node Config](environments/preview/config.json)
- [DB Sync Config](environments/preview/db-sync-config.json)
- [Submit API Config](environments/preview/submit-api-config.json)
- [Node Topology](environments/preview/topology.json)
- [Byron Genesis](environments/preview/byron-genesis.json)
- [Shelley Genesis](environments/preview/shelley-genesis.json)
- [Alonzo Genesis](environments/preview/alonzo-genesis.json)
- [Conway Genesis](environments/preview/conway-genesis.json)

## Pre-Production Testnet

Usage: Testing release candidates and mainnet releases. Forks at approximately same time as mainnet (within an epoch of each other).

Long running. Since this parallels mainnet, if a bug occurs here, it needs fixed properly and can not be respun.

Upgrade Strategy:

- Release Candidates - 1/3 of nodes
- Official Releases - 2/3 of nodes
- Hard forks - all nodes
- Community requested to only deploy release candidates and official releases

Changes Requested by: Release Squad Lead
Approvals Required: SRE Tribe Lead, Cardano Head of Engineering, Cardano Head of Architecture, CF Representative

Responsible: IOG SRE
Accountable: Head of SRE/Release Squad Lead
Consulted: SPOs, IOG Tribes
Informed: Cardano Core Tribe, COO, Director of Engineering, VP Community

- [Node Config](environments/preprod/config.json)
- [DB Sync Config](environments/preprod/db-sync-config.json)
- [Submit API Config](environments/preprod/submit-api-config.json)
- [Node Topology](environments/preprod/topology.json)
- [Byron Genesis](environments/preprod/byron-genesis.json)
- [Shelley Genesis](environments/preprod/shelley-genesis.json)
- [Alonzo Genesis](environments/preprod/alonzo-genesis.json)
- [Conway Genesis](environments/preprod/conway-genesis.json)

## Production (Mainnet)

Usage: Live Production. Only gets official mainnet releases.

Upgrade Strategy:

- Official Releases - Deploy 1 pool and it's relays every 24 hours
- Community requested to only deploy official releases


Changes Requested by: Release Squad Lead
Approvals Required: SRE Tribe Lead, IOG Executive Team, CF Executive Team

Responsible: IOG SRE
Accountable: Head of SRE/Release Squad Lead
Consulted: SPOs, IOG Tribes, IOG Executive Team
Informed: Cardano Core Tribe, COO, IOG Director of Engineering, IOG VP Community

- [Node Config](environments/mainnet/config.json)
- [DB Sync Config](environments/mainnet/db-sync-config.json)
- [Submit API Config](environments/mainnet/submit-api-config.json)
- [Node Topology](environments/mainnet/topology.json)
- [Node Topology (P2P - SPOs only!)](environments/mainnet/topology-p2p.json)
- [Byron Genesis](environments/mainnet/byron-genesis.json)
- [Shelley Genesis](environments/mainnet/shelley-genesis.json)
- [Alonzo Genesis](environments/mainnet/alonzo-genesis.json)
- [Conway Genesis](environments/mainnet/conway-genesis.json)
