# `crates/ggen-cheat-scanner/tests/scanner_test.rs`

Source SHA-256: `1ccebbd92d2a555ed881d3fad1f42e727a2cfef2a39121e5ec49b9796d580ba2`

```mermaid
classDiagram
    class fn_fixture {
      <<fn>>
    }
    class fn_rule_ids {
      <<fn>>
    }
    class fn_cheat_t01_vacuous_assert_flags_positive_fixture {
      <<fn>>
    }
    class fn_cheat_t01_vacuous_assert_does_not_flag_clean_fixture {
      <<fn>>
    }
    class fn_cheat_t02_tautological_result_check_flags_positive_fixture {
      <<fn>>
    }
    class fn_cheat_t02_tautological_result_check_does_not_flag_clean_fixture {
      <<fn>>
    }
    class fn_cheat_t03_no_assertion_test_flags_positive_fixture {
      <<fn>>
    }
    class fn_cheat_t03_no_assertion_test_does_not_flag_clean_fixture {
      <<fn>>
    }
    class fn_cheat_t04_mock_import_flags_positive_fixture {
      <<fn>>
    }
    class fn_cheat_t04_mock_import_does_not_flag_clean_fixture {
      <<fn>>
    }
    class fn_cheat_t04_mock_substitute_flags_cross_file_positive_pair {
      <<fn>>
    }
    class fn_cheat_t04_mock_substitute_does_not_flag_lone_trait_shape_stub {
      <<fn>>
    }
    class fn_cheat_t03_does_not_flag_should_panic_test {
      <<fn>>
    }
    class fn_cheat_t03_does_not_flag_try_operator_result_test {
      <<fn>>
    }
    class fn_cheat_t03_does_not_flag_assert_helper_delegation {
      <<fn>>
    }
    class fn_cheat_t04_does_not_flag_shared_ubiquitous_std_trait {
      <<fn>>
    }
```

## Dependencies

- `ggen_cheat_scanner::{collect_impls, find_mock_substitutes, scan_source}`
- `std::fs`
- `std::path::Path`

## Standing

- Structural parse: `ALIVE`
- Runtime behavior: `UNKNOWN`
