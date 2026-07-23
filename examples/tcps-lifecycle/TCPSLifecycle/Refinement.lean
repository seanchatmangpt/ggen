import TCPSLifecycle.Basic
import TCPSLifecycle.Autonomics

namespace TCPSLifecycle.Refinement

inductive RustDecision where
  | admit
  | plan
  | authorize
  | executeGreen
  | executeAbnormal
  | recoverUpdated
  | recoverUnchanged
  deriving DecidableEq, Repr

def rustStep : TCPSLifecycle.State → RustDecision → Option TCPSLifecycle.State
  | .observed, .admit => some .admitted
  | .admitted, .plan => some .planned
  | .planned, .authorize => some .authorized
  | .authorized, .executeGreen => some .receipted
  | .authorized, .executeAbnormal => some .stopped
  | .stopped, .recoverUpdated => some .observed
  | _, _ => none

theorem rustStep_sound {from to : TCPSLifecycle.State} {decision : RustDecision}
    (h : rustStep from decision = some to) :
    TCPSLifecycle.Transition from to := by
  cases from <;> cases decision <;> simp [rustStep] at h
  all_goals cases h
  · exact .admit
  · exact .plan
  · exact .authorize
  · exact .executeReceipted
  · exact .stopOnAbnormality
  · exact .recoverWithUpdatedStandard

theorem rust_success_is_receipted {state : TCPSLifecycle.State}
    (h : rustStep .authorized .executeGreen = some state) :
    state = .receipted := by
  simpa [rustStep] using h.symm

theorem unchanged_recovery_is_refused :
    rustStep .stopped .recoverUnchanged = none := by
  rfl

inductive RustPhaseRegime where
  | continuous
  | phaseShift1000x
  deriving DecidableEq, Repr

def rustClassifyPhase (baseline observed : Nat) : Option RustPhaseRegime :=
  if baseline = 0 then
    none
  else if observed / baseline ≥ TCPSLifecycle.Autonomics.phaseShiftMultiplier then
    some .phaseShift1000x
  else
    some .continuous

theorem rust_999_9x_remains_continuous :
    rustClassifyPhase 10 9999 = some .continuous := by
  decide

theorem rust_1000x_enters_phase_shift :
    rustClassifyPhase 10 10000 = some .phaseShift1000x := by
  decide

theorem rust_zero_baseline_is_refused :
    rustClassifyPhase 0 10000 = none := by
  decide

/-!
`rustStep` and `rustClassifyPhase` are the expected pure models of
`aeneas-kernel/src/lib.rs`. Crown standing additionally requires
`scripts/verify-aeneas.sh` to regenerate the model from Rust and prove the
generated definitions extensionally equal to these targets. Extraction without
that binding is explicitly insufficient.
-/

end TCPSLifecycle.Refinement
