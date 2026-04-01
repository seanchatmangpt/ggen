# ggen-macros — Crate Audit

**Path:** `crates/ggen-macros/`
**Lines:** 5 proc macros
**Role:** Procedural macros for guards, bundles, compile-time ontology/template includes

---

## STATUS: MOSTLY DEAD

**Zero consumers.** No other workspace crate depends on `ggen-macros`.

### Macro Inventory

| Macro | Type | Status |
|-------|------|--------|
| `#[derive(Guard)]` | Derive | Active — generates Guard trait impls. Only used in own tests. |
| `#[derive(Bundle)]` | Derive | Active — generates bundle helpers. Only used in own tests. |
| `include_ontology!()` | Proc macro | `#[allow(dead_code)]` — "FUTURE: v4.0" |
| `include_templates!()` | Proc macro | `#[allow(dead_code)]` — "FUTURE: v4.0" |
| `include_examples!()` | Proc macro | `#[allow(dead_code)]` — "FUTURE: v4.0" |
| `#[require_guards]` | Attribute | `#[allow(dead_code)]` — "FUTURE: v4.0" |

3 of 5 macros are explicitly dead-code stubs deferred to v4.0. The 2 active macros are only tested within `ggen-macros` itself.

The `Guard` derive has a hard dependency on consuming crate having specific internal modules (`crate::guards::Guard`, `crate::error::MarketplaceError`), making it tightly coupled to marketplace crate internals.

---

## FIX / DELETE / REFACTOR

| Action | Item | Priority |
|--------|------|----------|
| **DECIDE** | If macros will be used in v4.0, keep. Otherwise remove. | P3 |
| **DELETE** | 3 dead macros (include_*, require_guards) if not planning v4.0 | P3 |
