/**
 * ECOSYSTEM-SPEC-27.1.1 Gate 1 D5 — zcode-cli ui-layout-court contract stub.
 *
 * OWNER: seanchatmangpt/zcode-cli (out of the ggen workspace).
 * STANDING in ggen: UNSUPPORTED / BLOCKED for the TypeScript runner itself.
 *
 * This file documents the wire shape ggen's Rust adapter
 * (`ggen_graph::layout::ui_layout`) admits. zcode-cli SHALL:
 *   1. emit LayoutCourtCase JSON (or equivalent) for candidate UI trees;
 *   2. consume AotSealReceipt + seal_digest without re-implementing geometry;
 *   3. fail closed on missing / stale / mismatched digests;
 *   4. never re-authorize DO.
 *
 * In-repo falsifier (ggen fence only):
 *   cargo test -p ggen-graph --test ui_layout_court
 *
 * Next verifier when zcode-cli is mounted:
 *   pnpm test -- ui-layout-court   # (or that repo's exact court runner)
 * against docs/rfc/v27.1.1/fixtures/ui-layout-valid.column.json
 */

export const LAYOUT_COURT_NOUN = "ui-layout-court" as const;
export const LAYOUT_COURT_SCHEMA_V1 = "ggen.layout.aot.column/v1" as const;

export type AotLeaf = {
  id: string;
  width_px: number;
  height_px: number;
  is_text: boolean;
  is_interactive: boolean;
};

export type AotColumn = {
  gap_px: number;
  children: AotLeaf[];
};

export type LayoutCourtCase = {
  court: typeof LAYOUT_COURT_NOUN;
  schema: typeof LAYOUT_COURT_SCHEMA_V1;
  text_spacing: number;
  column: AotColumn;
  expected_seal_digest?: string;
};

/**
 * Placeholder: real zcode-cli court loads a ggen seal receipt and compares digests.
 * Do not claim ALIVE from this stub — it is not executed inside ggen.
 */
export function uiLayoutCourt(_case: LayoutCourtCase): "BLOCKED_ZCODE_CLI_OUT_OF_REPO" {
  return "BLOCKED_ZCODE_CLI_OUT_OF_REPO";
}
