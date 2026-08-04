# `examples/receiptctl/tests/chicago_tdd_tools_boundary_proof.rs`

Source SHA-256: `17e30c47d5bc7cedd1f919ede9ce4670dfd1b57b278f4b229a6f7fe337fb446e`

```mermaid
classDiagram
    class struct_ExpectedBoundaryTest {
      <<struct>>
      +"fn_signature: &'static str"
      +"exit_code_field: &'static str"
      +"stdout_field: Option~&'static str~"
      +"stderr_field: Option~&'static str~"
      +"axiom_doc: &'static str"
    }
    class fn_read_generated_boundary_file {
      <<fn>>
    }
    class fn_generated_file_has_expected_boundary_test_count {
      <<fn>>
    }
    class fn_generated_file_covers_every_query_derived_axiom {
      <<fn>>
    }
    class fn_read_generated_runtime_file {
      <<fn>>
    }
    class fn_generated_boundary_tests_reuse_one_dispatch_module_not_per_row_duplication {
      <<fn>>
    }
    class fn_generated_file_uses_real_cli_harness_not_a_mock {
      <<fn>>
    }
```

## Dependencies

- `std::env`
- `std::fs`
- `std::path::PathBuf`

## Standing

- Structural parse: `ALIVE`
- Runtime behavior: `UNKNOWN`
