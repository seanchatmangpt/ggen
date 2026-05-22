<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->
**Table of Contents**

- [mcpp.toml Specification](#mcpptoml-specification)
  - [Overview](#overview)
  - [Configuration Schema](#configuration-schema)
  - [Naming Conventions](#naming-conventions)
  - [Role in the God Box](#role-in-the-god-box)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

# mcpp.toml Specification

## Overview
The `mcpp.toml` file is the operational anchor for any MCPP-enabled repository. It defines the mapping between the public ontology (O*) and the local execution substrate. It serves as the "source of truth" for the MCPP CLI, directing how commands are lowered into actions.

## Configuration Schema

```toml
[project]
name = "mcpp-core"
public_name = "MCP Plus"
version = "0.1.0"
description = "Public-ontology-backed MCP Plus control substrate"

[ontology]
# Public profile mappings
profiles = [
    "https://chatmangpt.com/ns/mcpp",
    "http://www.w3.org/ns/odrl/2/",
    "http://www.w3.org/ns/prov#"
]
# Local source of truth (Turtle)
source = ".specify/mcpp.ttl"

[capabilities]
# Archetype enablement
doctor = { enabled = true, diagnostics = "standard" }
wizard = { enabled = true, profiles = ["receipts-first"] }
telco = { enabled = true, bridge_protocol = "a2a" }

[execution]
# Process ISA lowering target
isa = "powl8"
# Deterministic mode (enforces receipts and SHACL)
deterministic = true

[receipts]
# Evidence configuration
format = "spdx-prov"
storage = ".mcpp/receipts/"
auto_verify = true

[mcp_servers]
# Registered MCP servers managed by MCPP
[mcp_servers.ggen]
command = "mcpp"
args = ["mcp", "start-server"]
```

## Naming Conventions
- **Machine Name**: Always `mcpp`.
- **Public Name**: Always `MCP Plus`.
- **Command Grammar**: Noun-Verb (`mcpp doctor check`).

## Role in the God Box
In the "20 Item God Box," `mcpp.toml` is the closure item that converts fragmented project knowledge into an organized, executable repo. It is the peer to `ggen.toml` but focused on the **control surface** and **agent orchestration** rather than general code generation.
