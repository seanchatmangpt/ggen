# PR #849 integrate receipt (PARTIAL)

- **integrate tip:** `7e15cfb77404aca4665a2d467e73444050271d8f`
- **branch:** `cursor/ecosystem-spec-27-1-1-bc54` via `cursor/v2711-integrate-849-bc54`
- **conflicts:** none (all ort merges clean; gate0+photo auto-merged matrix/spec)

## Merged sibling tips

| Branch | SHA | Notes |
|--------|-----|-------|
| (base Gate-1 executable stack) | `16239f823` | shui + taffy stub + brand rhythm + six-width + CI-fix |
| `cursor/v2711-castle-spine-bc54` | `ceee5cc87` | docs ledger |
| `cursor/v2711-foundry-finish-bc54` | `85c71a559` | docs ledger + cross-links |
| `cursor/v2711-gate3-release-courts-bc54` | `e223e4cd8` | docs ledger |
| `cursor/v2711-revops-cos-bc54` | `047bdcff8` | docs ledger |
| `cursor/v2711-smalt-raid-bc54` | `0140a3a4b` | docs ledger |
| `cursor/v2711-gate0-billing-bc54` | `f33078a2d` | docs ledger |
| `cursor/v2711-photo-ledger-bc54` | `c4245fc2c` | docs ledger |
| `cursor/v2711-ccs-authority-bc54` | `04577c348` | docs ledger |
| `cursor/v2711-igniter-cleanroom-bc54` | `60a3bfe5b` | docs + `igniter_cleanroom_court` (executable) |

Taffy six-width (`22d108f54`) was already on remote ecosystem tip (IDLE agent); merged by inclusion, not mid-flight skip.

## Missing at integrate time (PARTIAL)

brand-fluid-clamp, ui-layout-court, verify_witness, shui-truex, layout-refusal-parity,
R-01..R-07, brand-fixtures, BRCE/WebAuthn, zcode-powl-ocel, gate1-gap-matrix (no unique remote tip).

## Gate-1 court exits (this pass)

Prebuilt `/workspace/target` binaries (tip-adjacent; six-width source on tip has 6 tests):

| Court | Exit | Result |
|-------|------|--------|
| `shui_public_prefix_court` | 0 | 3 passed |
| `taffy_aot_preflight_court` (prebuilt) | 0 | 4 passed (pre-six-width binary) |
| `brand_toml_rhythm_court` | 0 | 4 passed |

Cold recompile of tip blocked: `oxrocksdb-sys` via sccache/`c++` missing STL headers (`BUILD_BROKEN` in this pod for fresh target dirs). Aggregate Gate-1 standing remains **PARTIAL_ALIVE**.

## PR body update

`ManagePullRequest` unavailable in this subagent session; `gh pr edit` / REST PATCH / `gh pr comment` all returned **403 Resource not accessible by integration**. Inventory recorded here + push to PR head instead.
