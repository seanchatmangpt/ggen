# `examples/receiptctl/tests/w4pm_cognition_proof.rs`

Source SHA-256: `2455c06affad9460e7216c4ba9c20bba7dcdaaa80d28946f7e37978b05bb3248`

```mermaid
classDiagram
    class mod_w4pm_cognition_catalog {
      <<mod>>
    }
    class mod_w4pm_cognition_dispatch_handler {
      <<mod>>
    }
    class mod_w4pm_cognition_dispatch {
      <<mod>>
    }
    class fn_breed_catalog_has_fifty_five_entries {
      <<fn>>
    }
    class fn_breed_strips_matches_ontology {
      <<fn>>
    }
    class fn_breed_mycin_matches_ontology {
      <<fn>>
    }
    class fn_breed_eliza_matches_ontology {
      <<fn>>
    }
    class fn_breed_bayesian_network_matches_ontology {
      <<fn>>
    }
    class fn_breed_act_r_matches_ontology {
      <<fn>>
    }
    class fn_breed_sat_cdcl_matches_ontology {
      <<fn>>
    }
    class fn_breed_ocpm_route_discoverer_matches_ontology {
      <<fn>>
    }
    class fn_breed_triz_matches_ontology {
      <<fn>>
    }
    class fn_from_breed_id_rejects_unknown_id {
      <<fn>>
    }
    class fn_dispatch_cognition_run_fails_closed_for_strips {
      <<fn>>
    }
    class fn_run_strips_and_run_mycin_do_not_cross_contaminate_breed_ids {
      <<fn>>
    }
```

## Dependencies

- `w4pm_cognition_catalog::{from_breed_id, CognitionBreedId, BREED_CATALOG}`
- `w4pm_cognition_dispatch::{dispatch_cognition_run, run_mycin, run_strips}`

## Standing

- Structural parse: `ALIVE`
- Runtime behavior: `UNKNOWN`
