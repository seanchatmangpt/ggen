# ggen-layout-court

CLI subject for zcode-cli `UiLayoutCourtAdapter` (`cargo run -p ggen-layout-court --bin layout_court`).

Consumes:

1. **product-subject** JSON (Taffy flex tree)
2. **LayoutCourtCase** / Gate-1 column JSON via `ggen_graph::layout::ui_layout`

| Exit | Meaning |
|------|---------|
| 0 | PASS / ALIVE |
| 1 | FAIL_REFUSED |
| 2 | SUBJECT_MISSING |

```bash
cargo run -p ggen-layout-court --bin layout_court -- fixtures/ui-layout-court/product-subject.json
```
