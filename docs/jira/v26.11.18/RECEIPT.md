# RECEIPT v26.11.18 (ggen sjira slice)
Standing: PARTIAL

## WP9 KernelDifferential

Standing: VERIFIED — `receipts/wp9-kernel-differential.md` (also mirrored under
`docs/sjira/v26.11.18/receipts/WP9-KernelDifferential.md` and fnox
`docs/sjira/v26.11.18/receipts/WP9-KernelDifferential.md`).

Evidence: mutual admit digests
`sha256:94e00b6ed077be2d5e9e8b799ccbc480d0a1d5350b01ee86b15118b951ae4184`;
focused cargo EXIT=0 (`projection_test` 12, `shared_fixture_admission_test` 1,
`work_order_admission_test` 15); `falsifiers: []` + `path_scope: ["lib/"]`.

File standing stays PARTIAL: PATH-installed `ggen sjira admit` still refuses
`authority_requirement` (stale binary); other v26.11.18 wave work open.
