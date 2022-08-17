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

### Vasil-Dev

- [Node Config](environments/vasil-dev/config.json)
- [DB Sync Config](environments/vasil-dev/db-sync-config.json)
- [Submit API Config](environments/vasil-dev/submit-api-config.json)
- [Node Topology](environments/vasil-dev/topology.json)
- [Byron Genesis](environments/vasil-dev/byron-genesis.json)
- [Shelley Genesis](environments/vasil-dev/shelley-genesis.json)
- [Alonzo Genesis](environments/vasil-dev/alonzo-genesis.json)

## Shelley-QA (Internal Only)

Usage: Testing all new functionality. Ideally automatically deployed off of master on every commit

Ideally, stays long running to build up history, although the nature of early testing can potentially
break it. In the past we have disaster recoveried it since we control all the keys.

Upgrade Strategy: Deploy all nodes with every request

Changes Requested by: QA Team
Approvals Required: SRE Director or SRE Resident Manager

Responsible: SRE
Accountable: SRE Director/QA Manager
Consulted: Release Manager
Informed: QA Team

- [Node Config](environments/shelley-qa/config.json)
- [DB Sync Config](environments/shelley-qa/db-sync-config.json)
- [Submit API Config](environments/shelley-qa/submit-api-config.json)
- [Node Topology](environments/shelley-qa/topology.json)
- [Byron Genesis](environments/shelley-qa/byron-genesis.json)
- [Shelley Genesis](environments/shelley-qa/shelley-genesis.json)
- [Alonzo Genesis](environments/shelley-qa/alonzo-genesis.json)

## Preview Testnet

Usage: Testing release candidates and mainnet releases. Leads mainnet hard forks by at least 4 weeks.

Ideally stays long running. Only if an issue is found after it forks that's breaking should it be respun.

Epoch length of 1 day. Development flags allowed in configuration files.

Upgrade Strategy:

- Release Candidates - 1/3 of nodes
- Official Releases - 2/3 of nodes
- Hard forks - all nodes
- Community requested to only deploy release candidates and official releases

Changes Requested by: Release Manager
Approvals Required: SRE Director, Head of Delivery

Responsible: IOG SRE, CF SRE
Accountable: SRE Director/Release Manager
Consulted: Release Manager
Informed: Cardano Core Tribe, COO, Director of Engineering

- [Node Config](environments/preview/config.json)
- [DB Sync Config](environments/preview/db-sync-config.json)
- [Submit API Config](environments/preview/submit-api-config.json)
- [Node Topology](environments/preview/topology.json)
- [Byron Genesis](environments/preview/byron-genesis.json)
- [Shelley Genesis](environments/preview/shelley-genesis.json)
- [Alonzo Genesis](environments/preview/alonzo-genesis.json)

## Pre-Production Testnet

Usage: Testing release candidates and mainnet releases. Forks at approximately same time as mainnet (within an epoch of each other).

Long running. Since this parallels mainnet, if a bug occurs here, it needs fixed properly and can not be respun.

Upgrade Strategy:

- Release Candidates - 1/3 of nodes
- Official Releases - 2/3 of nodes
- Hard forks - all nodes
- Community requested to only deploy release candidates and official releases

Changes Requested by: Release Manager
Approvals Required: SRE Director, Head of Delivery, COO, Director of Engineering, VP Community

Responsible: IOG SRE, CF SRE
Accountable: SRE Director/QA Manager
Consulted: Release Manager
Informed: Cardano Core Tribe, COO, Director of Engineering, VP Community

- [Node Config](environments/preprod/config.json)
- [DB Sync Config](environments/preprod/db-sync-config.json)
- [Submit API Config](environments/preprod/submit-api-config.json)
- [Node Topology](environments/preprod/topology.json)
- [Byron Genesis](environments/preprod/byron-genesis.json)
- [Shelley Genesis](environments/preprod/shelley-genesis.json)
- [Alonzo Genesis](environments/preprod/alonzo-genesis.json)

## Production (Mainnet)

Usage: Live Production. Only gets official mainnet releases.

Upgrade Strategy:

- Official Releases - Deploy 1 pool and it's relays every 24 hours
- Community requested to only deploy official releases

Changes Requested by: Release Manager, Tribe Leads
Approvals Required: CEO, COO, SRE Director, Head of Delivery, Director of Engineering, VP Community

Responsible: IOG SRE, CF SRE
Accountable: SRE Director/QA Manager
Consulted: Release Manager
Informed: Cardano Core Tribe, COO, Director of Engineering, VP Community

- [Node Config](environments/mainnet/config.json)
- [DB Sync Config](environments/mainnet/db-sync-config.json)
- [Submit API Config](environments/mainnet/submit-api-config.json)
- [Node Topology](environments/mainnet/topology.json)
- [Byron Genesis](environments/mainnet/byron-genesis.json)
- [Shelley Genesis](environments/mainnet/shelley-genesis.json)
- [Alonzo Genesis](environments/mainnet/alonzo-genesis.json)

## Legacy

### Testnet

- [Node Config](environments/testnet/config.json)
- [Node Topology](environments/testnet/topology.json)
- [Byron Genesis](environments/testnet/byron-genesis.json)
- [Shelley Genesis](environments/testnet/shelley-genesis.json)
- [Alonzo Genesis](environments/testnet/alonzo-genesis.json)

### Staging (Internal Only)

- [Node Config](environments/staging/config.json)
- [Node Topology](environments/staging/topology.json)
- [Byron Genesis](environments/staging/byron-genesis.json)
- [Shelley Genesis](environments/staging/shelley-genesis.json)
- [Alonzo Genesis](environments/staging/alonzo-genesis.json)
