import CrownFormal.Operational

set_option autoImplicit false

/-!
# Component-complete planning semantics

Lawfulness is a conjunction over replay, goal, precondition, invariant,
numeric, temporal, and trajectory components. Each component is connected to
an explicit adjacent-swap preservation theorem. A separate standing receipt
forces every all-accepting component to be declared neutral rather than being
silently mistaken for evidence.
-/

namespace CrownFormal

universe u v

/-- Predicate invariance under one admitted adjacent commutation. -/
def SwapInvariant {Action : Type v} (I : Independence Action)
    (predicate : List Action → Prop) : Prop :=
  ∀ pre a b suf,
    I.independent a b →
      (predicate (pre ++ [a, b] ++ suf) ↔
       predicate (pre ++ [b, a] ++ suf))

/-- A list predicate invariant under generators is invariant under the full
trace congruence. -/
theorem swapInvariant_trace_iff {Action : Type v} {I : Independence Action}
    {predicate : List Action → Prop}
    (invariant : SwapInvariant I predicate)
    {left right : List Action} (equivalent : TraceEq I left right) :
    predicate left ↔ predicate right := by
  induction equivalent with
  | refl word => exact Iff.rfl
  | swap pre a b suf independent =>
      exact invariant pre a b suf independent
  | symm h ih => exact ih.symm
  | trans h₁ h₂ ih₁ ih₂ => exact ih₁.trans ih₂

/-- Executable planning semantics with all PDDL-relevant lawfulness layers. -/
structure PlanningSemantics (State : Type u) (Action : Type v) where
  system : TransitionSystem State Action
  initial : State
  goal : State → Prop
  preconditions : List Action → Prop
  invariants : List Action → Prop
  numericFlows : List Action → Prop
  temporal : List Action → Prop
  trajectory : List Action → Prop

namespace PlanningSemantics

variable {State : Type u} {Action : Type v}

/-- A behavior is lawful only when replay succeeds, its final state is a goal,
and every admitted semantic component accepts it. -/
def Lawful (semantics : PlanningSemantics State Action)
    (word : List Action) : Prop :=
  ∃ final,
    semantics.system.run semantics.initial word = some final ∧
    semantics.goal final ∧
    semantics.preconditions word ∧
    semantics.invariants word ∧
    semantics.numericFlows word ∧
    semantics.temporal word ∧
    semantics.trajectory word

end PlanningSemantics

/-- Constructive preservation certificate for every semantic component. -/
structure SemanticIndependenceCertificate
    {State : Type u} {Action : Type v}
    (semantics : PlanningSemantics State Action)
    (I : Independence Action) where
  effectDiamond : DiamondCertificate semantics.system I
  preconditionsStable : SwapInvariant I semantics.preconditions
  invariantsStable : SwapInvariant I semantics.invariants
  numericFlowsStable : SwapInvariant I semantics.numericFlows
  temporalStable : SwapInvariant I semantics.temporal
  trajectoryStable : SwapInvariant I semantics.trajectory

namespace SemanticIndependenceCertificate

variable {State : Type u} {Action : Type v}
variable {semantics : PlanningSemantics State Action}
variable {I : Independence Action}

/-- Full lawfulness is invariant under Mazurkiewicz trace equivalence. -/
theorem lawful_trace_iff
    (certificate : SemanticIndependenceCertificate semantics I)
    {left right : List Action} (equivalent : TraceEq I left right) :
    semantics.Lawful left ↔ semantics.Lawful right := by
  have replay := certificate.effectDiamond.run_trace_eq semantics.initial equivalent
  have preconditions :=
    swapInvariant_trace_iff certificate.preconditionsStable equivalent
  have invariants :=
    swapInvariant_trace_iff certificate.invariantsStable equivalent
  have numeric :=
    swapInvariant_trace_iff certificate.numericFlowsStable equivalent
  have temporal :=
    swapInvariant_trace_iff certificate.temporalStable equivalent
  have trajectory :=
    swapInvariant_trace_iff certificate.trajectoryStable equivalent
  constructor
  · rintro ⟨final, run, goal, pre, inv, num, time, traj⟩
    refine ⟨final, ?_, goal, preconditions.mp pre, invariants.mp inv,
      numeric.mp num, temporal.mp time, trajectory.mp traj⟩
    calc
      semantics.system.run semantics.initial right =
          semantics.system.run semantics.initial left := replay.symm
      _ = some final := run
  · rintro ⟨final, run, goal, pre, inv, num, time, traj⟩
    refine ⟨final, ?_, goal, preconditions.mpr pre, invariants.mpr inv,
      numeric.mpr num, temporal.mpr time, trajectory.mpr traj⟩
    calc
      semantics.system.run semantics.initial left =
          semantics.system.run semantics.initial right := replay
      _ = some final := run

/-- Directional form matching the original crown obligation. -/
theorem traceSwapPreservesLawful
    (certificate : SemanticIndependenceCertificate semantics I)
    {left right : List Action} (equivalent : TraceEq I left right)
    (lawful : semantics.Lawful left) :
    semantics.Lawful right :=
  (certificate.lawful_trace_iff equivalent).mp lawful

end SemanticIndependenceCertificate

/-- Explicit standing of a semantic component. An unconstrained component is
legal only when declared neutral. An active component must carry both an
acceptance and a rejection witness. -/
inductive ComponentStanding {Action : Type v}
    (predicate : List Action → Prop) : Type v
  | neutral (allAccept : ∀ word, predicate word)
  | active
      (acceptedWord : List Action)
      (accepted : predicate acceptedWord)
      (rejectedWord : List Action)
      (rejected : ¬ predicate rejectedWord)

/-- Evidence ledger for all non-transition semantic layers. -/
structure SemanticBoundaryReceipt
    {State : Type u} {Action : Type v}
    (semantics : PlanningSemantics State Action) where
  preconditions : ComponentStanding semantics.preconditions
  invariants : ComponentStanding semantics.invariants
  numericFlows : ComponentStanding semantics.numericFlows
  temporal : ComponentStanding semantics.temporal
  trajectory : ComponentStanding semantics.trajectory

end CrownFormal
