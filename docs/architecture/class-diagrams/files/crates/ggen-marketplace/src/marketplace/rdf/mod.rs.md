# `crates/ggen-marketplace/src/marketplace/rdf/mod.rs`

Source SHA-256: `a4d8190accd9a925d23b2f95de873131c44bb181396e537b3b53f3e120152741`

```mermaid
classDiagram
    class mod_control {
      <<mod>>
    }
    class mod_fmea_mitigations {
      <<mod>>
    }
    class mod_ontology {
      <<mod>>
    }
    class mod_poka_yoke {
      <<mod>>
    }
    class mod_rdf_control {
      <<mod>>
    }
    class mod_sparql {
      <<mod>>
    }
    class mod_sparql_queries {
      <<mod>>
    }
    class mod_state_machine {
      <<mod>>
    }
    class mod_turtle_config {
      <<mod>>
    }
```

## Dependencies

- `control::RdfControlPlane`
- `fmea_mitigations::{FailureCategory, FailureMode, FmeaMitigationManager, MitigationResult}`
- `ontology::{generate_prefixes, namespaces}`
- `poka_yoke::{Literal, PokaYokeError, RdfGraph, ResourceId, Triple, ValidationConstraint}`
- `rdf_control::{ControlPlaneError, StateTransitionResult, ValidationResult}`
- `sparql::{SparqlExecutor, SparqlQuery, SparqlQueryBuilder}`
- `sparql_queries::{MarketplaceQueries, PackageSearchResult, SearchParams}`
- `state_machine::StateMachineExecutor`
- `turtle_config::TurtleConfigLoader`

## Standing

- Structural parse: `ALIVE`
- Runtime behavior: `UNKNOWN`
