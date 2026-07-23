import TCPSLifecycle.Basic

namespace TCPSLifecycle.Refinement

inductive RustDecision where
  | admit
  | plan
  | authorize
  | executeGreen
  | executeAbnormal
  | recoverWithUpdatedStandard
  deriving DecidableEq, Repr

def rustStep : TCPSLifecycle.State → RustDecision → Option TCPSLifecycle.State
  | .observed, .admit => some .admitted
  | .admitted, .plan => some .planned
  | .planned, .authorize => some .authorized
  | .authorized, .executeGreen => some .receipted
  | .authorized, .executeAbnormal => some .stopped
  | .stopped, .recoverWithUpdatedStandard => some .observed
  | _, _ => none

def CorrespondingDecision : RustDecision →
    TCPSLifecycle.State → TCPSLifecycle.State → Prop
  | .admit => TCPSLifecycle.Transition
  | .plan => TCPSLifecycle.Transition
  | .authorize => TCPSLifecycle.Transition
  | .executeGreen => TCPSLifecycle.Transition
  | .executeAbnormal => TCPSLifecycle.Transition
  | .recoverWithUpdatedStandard => TCPSLifecycle.Transition

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

/-!
`rustStep` is the expected pure model of `src/aeneas_kernel.rs`. Crown standing
requires `scripts/verify-aeneas.sh` to regenerate the model from Rust with
Charon/Aeneas and bind the generated symbols to this theorem. Until that
external receipt exists, this file proves the semantic target but does not claim
end-to-end extraction correspondence.
-/

end TCPSLifecycle.Refinement
