/**
 * TICKET-034: Monaco runtime adapter (custom).
 *
 * Implements the real Monaco Editor integration behind TICKET-031's
 * editor-shell.tsx generated props interface. TICKET-031 has not been
 * generated yet in this session (workstream H ran ahead of workstream
 * E/F/G's projection), so the "expected shape pending TICKET-031" interface
 * below is authored by hand, documented as pending, and MUST be reconciled
 * (not silently kept) once TICKET-031 actually generates editor-shell.tsx.
 *
 * Only the non-DOM logic (language-id mapping, options/config building) is
 * unit-testable here without a browser. Actually mounting `monaco-editor`
 * requires a DOM + web worker environment; that half needs Playwright
 * (TICKET-039) against a real browser, not a Node unit test — this file
 * states that honestly rather than mocking `monaco-editor`'s module.
 */
import { checkPolicy, DEFAULT_ACTIVE_MODE, type PolicyId } from "./policy-check-adapter";

/** PENDING(TICKET-031): expected shape of editor-shell.tsx's generated
 * props interface. Replace with the real generated import once it exists. */
export interface EditorShellProps {
  capability: "compile_python" | "execute_python" | "run_pytest" | "compile_rust" | "execute_rust" | "run_cargo_test";
  initialValue: string;
  readOnly?: boolean;
  /** Optional: see policy-check-adapter.ts's DEFAULT_ACTIVE_MODE doc. */
  activeMode?: PolicyId;
}

export type MonacoLanguageId = "python" | "rust";

/**
 * Pure, DOM-free mapping from a capability id (as carried on
 * EditorShellProps) to the Monaco language id the editor instance must be
 * configured with. No domain rule is reimplemented here — this is a
 * mechanical string mapping, not a policy decision.
 */
export function languageIdForCapability(capability: EditorShellProps["capability"]): MonacoLanguageId {
  if (capability.endsWith("_rust") || capability === "run_cargo_test") return "rust";
  return "python";
}

export interface MonacoEditorConfig {
  language: MonacoLanguageId;
  value: string;
  readOnly: boolean;
  automaticLayout: boolean;
  minimap: { enabled: boolean };
}

/**
 * Builds the config object passed to `monaco.editor.create(...)`. Calls the
 * (currently pending) policy check before returning a config that would
 * permit editing — mirrors every other adapter's "check before act"
 * requirement, even though "acting" here is just building a config object,
 * because a denied capability must never reach the editor as an editable
 * config in the first place.
 */
export function buildMonacoConfig(props: EditorShellProps): MonacoEditorConfig {
  const decision = checkPolicy(props.capability, props.activeMode ?? DEFAULT_ACTIVE_MODE);
  if (!decision.allowed) {
    throw new Error(`monaco-adapter refused: ${decision.reason ?? "policy denied"}`);
  }
  return {
    language: languageIdForCapability(props.capability),
    value: props.initialValue,
    readOnly: Boolean(props.readOnly),
    automaticLayout: true,
    minimap: { enabled: false },
  };
}

/**
 * Reduction path: if a future Monaco version exposes a fully declarative
 * headless config API, more of this adapter's worker-registration and
 * language-config wiring could migrate to generated configuration data
 * rather than this imperative `buildMonacoConfig` function. As of Monaco
 * 0.5x, editor creation and worker registration remain imperative
 * JavaScript calls against a real DOM node, so this boundary is currently
 * irreducible.
 */
export const REDUCTION_PATH_NOTE =
  "buildMonacoConfig/languageIdForCapability are DOM-free and unit-tested; " +
  "monaco.editor.create(...) + worker registration require a real browser " +
  "DOM and are exercised only via Playwright (TICKET-039), not here.";
