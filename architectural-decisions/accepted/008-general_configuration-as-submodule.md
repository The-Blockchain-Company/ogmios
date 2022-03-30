---
Number: 8
Title: Configuration as sub-module
Category: General
Status: accepted 
---

<!-- ADR template adapted from Michael Nygard's -->

## Context

<!-- What is the issue that we're seeing that is motivating this decision or change? -->

Ogmios works hand-and-hand with the [Cardano node](https://github.com/The-Blockchain-Company/bcc-node). Without a node, there isn't much going on with the Ogmios server. It therefore makes sense to not view them separately, but rather think about bcc-node-ogmios as a single container which happens to be a bcc-node with a WebSocket interface. While Ogmios is very lightweight and can be configured fully from a few command-line flags, bcc-node requires more complex configuration files (one for the node, multiple genesis files, keys, etc..). 

This is actually the case for many projects which directly depends on a node, like [bcc-rosetta](https://github.com/The-Blockchain-Company/bcc-rosetta), [bcc-graphql](https://github.com/The-Blockchain-Company/bcc-graphql) or [bcc-wallet](https://github.com/The-Blockchain-Company/bcc-wallet). 

## Decision

<!-- What is the change that we're proposing and/or doing? -->

We'll provide, as part of the repository architecture the necessary configuration files for bcc-node. Since this is a concern which can be found in other projects as well, we have decided to host the configurations in a separate repository, and depends on this repository as sub-module in order to:

1. Share a common structure across various projects
2. Provide an easy access to latest configuration 

## Consequences

<!-- What becomes easier or more difficult to do because of this change? -->

- [bcc-configurations](https://github.com/The-Blockchain-Company/bcc-configurations) was created, including a nightly job to automatically synchronize configurations from https://hydra.blockchain-company.io
- A git sub-module has been added to the Ogmios repository, which makes the latest configurations easy to reach in a single pull. 
