/**
 * Hand-authored (Phase 4 -- UI/UX redesign; no ontology resource backs this
 * component, same discipline as cognition-panel.tsx). Top-of-page session
 * status strip: which policy mode is active, the current session phase, a
 * one-word input status, and the entry point into the accessibility
 * preferences dialog (see accessibility-preferences-dialog.tsx).
 *
 * Deliberately dumb: owns no state, no fetch, no dispatch. `mode` is passed
 * as the raw PolicyId string (e.g. "policy/practice-mode") -- the display
 * transform below is the same mechanical ALL_CAPS/kebab -> Title Case
 * approach refusal-presentation.tsx already uses for RefusalCode, not a
 * hardcoded per-mode label table (avoids Epistemic Bypass per
 * .claude/rules/coding-agent-mistakes.md).
 *
 * Composes the existing GENERATED PhaseIndicator (TICKET-030) unchanged --
 * "session state" below is that real component, not a re-implementation.
 */
import { PhaseIndicator } from "./phase-indicator";
import type { Phase } from "../lib/domain/phase";

export type InputStatus = "idle" | "listening" | "processing";

export interface SessionHeaderProps {
  mode: string;
  phase: Phase;
  inputStatus: InputStatus;
  onOpenAccessibilityPreferences: () => void;
}

function displayFromResourceId(id: string): string {
  const leaf = id.includes("/") ? id.slice(id.lastIndexOf("/") + 1) : id;
  return leaf
    .split("-")
    .filter((w) => w.length > 0)
    .map((w) => w.charAt(0).toUpperCase() + w.slice(1))
    .join(" ");
}

const INPUT_STATUS_LABEL: Record<InputStatus, string> = {
  idle: "Idle",
  listening: "Listening",
  processing: "Processing",
};

export function SessionHeader({ mode, phase, inputStatus, onOpenAccessibilityPreferences }: SessionHeaderProps) {
  return (
    <header className="session-header" data-testid="session-header">
      <a href="#coding-region" className="skip-link" data-testid="skip-to-current-task">
        Skip to current task
      </a>
      <div className="session-header-row">
        <span className="badge badge-mode" data-testid="session-header-mode" data-mode={mode}>
          {displayFromResourceId(mode)}
        </span>

        <span className="session-header-status" data-testid="session-header-status">
          <span className="session-header-label">Session status</span>
          <PhaseIndicator phase={phase} />
        </span>

        <span
          className="badge badge-input-status"
          data-testid="session-header-input-status"
          data-status={inputStatus}
        >
          {INPUT_STATUS_LABEL[inputStatus]}
        </span>

        <button
          type="button"
          onClick={onOpenAccessibilityPreferences}
          data-testid="session-header-accessibility-button"
        >
          Accessibility preferences
        </button>
      </div>
    </header>
  );
}
