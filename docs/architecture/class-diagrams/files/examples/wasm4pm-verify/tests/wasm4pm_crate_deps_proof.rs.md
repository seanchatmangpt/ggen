# `examples/wasm4pm-verify/tests/wasm4pm_crate_deps_proof.rs`

Source SHA-256: `be675484efbdc52971fef1558e94705464b470e436b9a0d6b706a38aa3092cad`

```mermaid
classDiagram
    class mod_wasm4pm_crate_deps {
      <<mod>>
    }
    class fn_edge_count_matches_ontology {
      <<fn>>
    }
    class fn_direct_dependency_lookup {
      <<fn>>
    }
    class fn_wasm4pm_has_two_direct_dependencies {
      <<fn>>
    }
    class fn_leaf_crate_has_no_dependencies {
      <<fn>>
    }
    class fn_unknown_crate_name_returns_empty_not_panic {
      <<fn>>
    }
    class fn_transitive_closure_covers_multi_hop_reachable_set {
      <<fn>>
    }
    class fn_transitive_closure_of_leaf_is_empty {
      <<fn>>
    }
    class fn_no_edge_is_a_self_loop {
      <<fn>>
    }
    class fn_cycle_guard_terminates_on_a_synthetic_cycle {
      <<fn>>
    }
    class fn_transitive_dependencies_delegates_to_the_parameterized_form {
      <<fn>>
    }
```

## Dependencies

- `wasm4pm_crate_deps::{ depends_on, transitive_dependencies, transitive_dependencies_over, CrateDepEdge, CRATE_DEPS, }`

## Standing

- Structural parse: `ALIVE`
- Runtime behavior: `UNKNOWN`
