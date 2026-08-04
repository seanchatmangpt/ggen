# `tests/integration/ontology_workflows_hipaa.rs`

Source SHA-256: `85a51374b987a35f76de81fed142c9bd5021a4e86617d14ed2f03e6935a053ec`

```mermaid
classDiagram
    class fn_create_healthcare_provider_ontology {
      <<fn>>
    }
    class fn_create_healthcare_domain_yaml {
      <<fn>>
    }
    class fn_test_hipaa_compliance_workflow {
      <<fn>>
    }
    class fn_evaluate_hipaa_guards {
      <<fn>>
    }
    class fn_test_hipaa_guards_fail_on_missing_encryption {
      <<fn>>
    }
    class fn_test_hipaa_proposal_determinism_multiple_runs {
      <<fn>>
    }
    class fn_generate_hipaa_proposal {
      <<fn>>
    }
```

## Dependencies

- `sha2::{Digest, Sha256}`
- `std::collections::BTreeMap`
- `std::fs`
- `tempfile::TempDir`

## Standing

- Structural parse: `ALIVE`
- Runtime behavior: `UNKNOWN`
