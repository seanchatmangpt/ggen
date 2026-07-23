import Std

namespace TCPSLifecycle

inductive State where
  | observed
  | admitted
  | planned
  | authorized
  | receipted
  | stopped
  deriving DecidableEq, Repr

inductive Transition : State → State → Prop where
  | admit : Transition .observed .admitted
  | plan : Transition .admitted .planned
  | authorize : Transition .planned .authorized
  | executeReceipted : Transition .authorized .receipted
  | stopOnAbnormality : Transition .authorized .stopped
  | recoverWithUpdatedStandard : Transition .stopped .observed

structure Authorization where
  planDigest : String
  deriving DecidableEq, Repr

structure Receipt where
  observationId : String
  planDigest : String
  authorizationDigest : String
  artifactDigest : String
  receiptDigest : String
  deriving DecidableEq, Repr

inductive Outcome where
  | receipted (receipt : Receipt)
  | stopped (reason : String)
  deriving DecidableEq, Repr

def Successful : Outcome → Prop
  | .receipted _ => True
  | .stopped _ => False

theorem successful_has_receipt {outcome : Outcome} (h : Successful outcome) :
    ∃ receipt, outcome = .receipted receipt := by
  cases outcome with
  | receipted receipt => exact ⟨receipt, rfl⟩
  | stopped reason => cases h

theorem observed_cannot_execute_directly :
    ¬ Transition .observed .receipted := by
  intro h
  cases h

theorem admitted_cannot_execute_directly :
    ¬ Transition .admitted .receipted := by
  intro h
  cases h

theorem planned_requires_authorization :
    ¬ Transition .planned .receipted := by
  intro h
  cases h

def Corresponds (authorization : Authorization) (receipt : Receipt) : Prop :=
  authorization.planDigest = receipt.planDigest

theorem authorized_receipt_corresponds
    (observationId planDigest authorizationDigest artifactDigest receiptDigest : String) :
    Corresponds ⟨planDigest⟩
      ⟨observationId, planDigest, authorizationDigest, artifactDigest, receiptDigest⟩ := by
  rfl

def ValidRecovery (oldStandard newStandard : String) : Prop :=
  newStandard ≠ "" ∧ newStandard ≠ oldStandard

theorem unchanged_standard_cannot_restart (standard : String) :
    ¬ ValidRecovery standard standard := by
  intro h
  exact h.2 rfl

theorem empty_standard_cannot_restart (standard : String) :
    ¬ ValidRecovery standard "" := by
  intro h
  exact h.1 rfl

end TCPSLifecycle
