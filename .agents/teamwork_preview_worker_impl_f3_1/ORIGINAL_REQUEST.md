## 2026-06-06T13:43:51-07:00
Create the integration test file `/Users/sac/ggen/crates/ggen-projection/tests/f3_pack_proving.rs` with exactly 10 tests verifying Feature 3: E2E Pack Proving & Generation.

The 10 tests must cover:
Tier 1: Feature Coverage (5 tests):
1. `test_f3_t1_durable_pack_generation`
2. `test_f3_t1_receipt_verification`
3. `test_f3_t1_drift_detection`
4. `test_f3_t1_provenance_emission`
5. `test_f3_t1_non_destructive_staging`

Tier 2: Boundaries and Corner Cases (5 tests):
6. `test_f3_t2_corrupted_receipt`
7. `test_f3_t2_missing_template`
8. `test_f3_t2_invalid_variables`
9. `test_f3_t2_empty_output_dir`
10. `test_f3_t2_checksum_mismatch`

All tests should use actual types from `ggen_projection` and execute properly. Run `cargo test -p ggen-projection --test f3_pack_proving` to verify the tests compile and pass.
