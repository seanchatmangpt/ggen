/**
 * Hand-authored (Phase 4 -- UI/UX redesign; no ontology resource backs this
 * component). Replaces the prior always-visible inline 16-checkbox list
 * (AccessibilityControls rendered directly in the page body) with a modal
 * dialog opened from SessionHeader's "Accessibility preferences" button,
 * per the redesign brief's control-replacement table.
 *
 * Wraps the existing GENERATED AccessibilityControls (TICKET-033)
 * unchanged -- this component owns only the native <dialog> modal
 * mechanics (showModal()/close() via ref, since those are imperative-only
 * DOM APIs with no declarative React prop equivalent), not the 16
 * individual settings, which AccessibilityControls itself still renders
 * from the real RDF-derived AccessibilityDefaults keys.
 */
import { useEffect, useRef } from "react";
import { AccessibilityControls } from "./accessibility-controls";
import type { AccessibilityDefaults } from "../lib/accessibility/defaults";

export interface AccessibilityPreferencesDialogProps {
  open: boolean;
  settings: AccessibilityDefaults;
  onChange: (key: keyof AccessibilityDefaults, value: boolean) => void;
  onClose: () => void;
}

export function AccessibilityPreferencesDialog({
  open,
  settings,
  onChange,
  onClose,
}: AccessibilityPreferencesDialogProps) {
  const dialogRef = useRef<HTMLDialogElement | null>(null);

  useEffect(() => {
    const dialog = dialogRef.current;
    if (!dialog) return;
    if (open && !dialog.open) dialog.showModal();
    if (!open && dialog.open) dialog.close();
  }, [open]);

  return (
    <dialog
      ref={dialogRef}
      aria-label="Accessibility preferences"
      data-testid="accessibility-preferences-dialog"
      onClose={onClose}
    >
      <h2>Accessibility preferences</h2>
      <AccessibilityControls settings={settings} onChange={onChange} />
      <button type="button" onClick={onClose} data-testid="accessibility-preferences-close">
        Done
      </button>
    </dialog>
  );
}
