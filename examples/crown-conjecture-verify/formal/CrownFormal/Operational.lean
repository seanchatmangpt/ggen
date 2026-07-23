import CrownFormal.Trace

set_option autoImplicit false

/-!
# Operational trace semantics

The semantic bridge is a deterministic transition system and its replay
function. Independence is not represented by disconnected propositions: a
diamond certificate states equality of the two actual two-step replays.
-/

namespace CrownFormal

universe u v

/-- Deterministic partial transition system. -/
structure TransitionSystem (State : Type u) (Action : Type v) where
  step : State → Action → Option State

namespace TransitionSystem

variable {State : Type u} {Action : Type v}

/-- Replay an action word from a supplied state. -/
def run (system : TransitionSystem State Action) :
    State → List Action → Option State
  | state, [] => some state
  | state, action :: rest =>
      (system.step state action).bind fun next => system.run next rest

@[simp] theorem run_nil (system : TransitionSystem State Action) (state : State) :
    system.run state [] = some state := rfl

@[simp] theorem run_cons (system : TransitionSystem State Action)
    (state : State) (action : Action) (rest : List Action) :
    system.run state (action :: rest) =
      (system.step state action).bind fun next => system.run next rest := rfl

/-- Replay respects list concatenation. -/
theorem run_append (system : TransitionSystem State Action)
    (state : State) (left right : List Action) :
    system.run state (left ++ right) =
      (system.run state left).bind fun middle => system.run middle right := by
  induction left generalizing state with
  | nil => simp
  | cons action rest ih =>
      simp only [List.cons_append, run_cons]
      cases system.step state action with
      | none => simp
      | some next =>
          simpa using ih next

/-- Proof-relevant execution derivation retaining every transition receipt. -/
inductive Execution (system : TransitionSystem State Action) :
    State → List Action → State → Type (max u v)
  | nil (state : State) : Execution system state [] state
  | cons {state next final : State} {action : Action} {rest : List Action}
      (receipt : system.step state action = some next)
      (tail : Execution system next rest final) :
      Execution system state (action :: rest) final

/-- Every execution derivation replays to its recorded final state. -/
theorem Execution.run_eq {system : TransitionSystem State Action}
    {state final : State} {word : List Action}
    (execution : Execution system state word final) :
    system.run state word = some final := by
  induction execution with
  | nil state => rfl
  | cons receipt tail ih =>
      simp [run, receipt, ih]

/-- Every successful deterministic replay can be reified as a proof-relevant
execution. This closes the converse direction of `Execution.run_eq`: replay
receipts are not merely inspectable after construction; they are recoverable
from every admitted successful run. -/
def Execution.of_run_eq {system : TransitionSystem State Action}
    {state final : State} {word : List Action}
    (runResult : system.run state word = some final) :
    Execution system state word final := by
  induction word generalizing state with
  | nil =>
      have stateEq : state = final := by
        simpa using runResult
      subst final
      exact .nil state
  | cons action rest ih =>
      simp only [run_cons] at runResult
      cases stepResult : system.step state action with
      | none =>
          simp [stepResult] at runResult
      | some next =>
          simp [stepResult] at runResult
          exact .cons stepResult (ih runResult)

end TransitionSystem

/-- Operational commutation tied directly to replay. -/
structure DiamondCertificate {State : Type u} {Action : Type v}
    (system : TransitionSystem State Action) (I : Independence Action) where
  commute : ∀ state a b,
    I.independent a b →
    system.run state [a, b] = system.run state [b, a]

namespace DiamondCertificate

variable {State : Type u} {Action : Type v}
variable {system : TransitionSystem State Action} {I : Independence Action}

/-- Replay is invariant under every finite trace-equivalence derivation. -/
theorem run_trace_eq (certificate : DiamondCertificate system I)
    (state : State) {left right : List Action}
    (equivalent : TraceEq I left right) :
    system.run state left = system.run state right := by
  induction equivalent with
  | refl word => rfl
  | swap pre a b suf independent =>
      simp only [List.append_assoc]
      rw [system.run_append state pre ([a, b] ++ suf)]
      rw [system.run_append state pre ([b, a] ++ suf)]
      cases system.run state pre with
      | none => simp
      | some middle =>
          simp only [Option.bind_some]
          rw [system.run_append middle [a, b] suf]
          rw [system.run_append middle [b, a] suf]
          rw [certificate.commute middle a b independent]
  | symm h ih => exact ih.symm
  | trans h₁ h₂ ih₁ ih₂ => exact ih₁.trans ih₂

/-- Successful replay transports across trace equivalence with the same final
state, not merely an extensionally related state. -/
theorem successful_replay_iff (certificate : DiamondCertificate system I)
    (state final : State) {left right : List Action}
    (equivalent : TraceEq I left right) :
    system.run state left = some final ↔ system.run state right = some final := by
  rw [certificate.run_trace_eq state equivalent]

/-- A proof-relevant execution can be replayed through any admitted trace
commutation. The resulting derivation contains the transition receipts for the
new serialization and retains the exact final state. -/
def transport_execution (certificate : DiamondCertificate system I)
    {state final : State} {left right : List Action}
    (equivalent : TraceEq I left right)
    (execution : TransitionSystem.Execution system state left final) :
    TransitionSystem.Execution system state right final := by
  apply TransitionSystem.Execution.of_run_eq
  exact
    (certificate.successful_replay_iff state final equivalent).mp
      execution.run_eq

end DiamondCertificate

end CrownFormal
