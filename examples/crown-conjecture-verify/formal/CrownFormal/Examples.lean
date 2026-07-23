import CrownFormal.Falsifiers

set_option autoImplicit false

/-!
# Constructive admitted model

A concrete arithmetic system demonstrates that the certificate interface is
inhabited by real semantics, not only by abstract assumptions.
-/

namespace CrownFormal

/-- Two different additive actions commute. -/
def distinctNatIndependence : Independence Nat where
  independent := fun a b => a ≠ b
  symmetric := fun h => Ne.symm h
  irreflexive := fun a h => h rfl

/-- Additive transition semantics. -/
def additiveSystem : TransitionSystem Nat Nat where
  step := fun state action => some (state + action)

/-- Replay is addition of the word sum. -/
theorem additive_run (state : Nat) (word : List Nat) :
    additiveSystem.run state word = some (state + word.sum) := by
  induction word generalizing state with
  | nil => simp [TransitionSystem.run]
  | cons action rest ih =>
      simp [TransitionSystem.run, additiveSystem, ih, Nat.add_assoc]

/-- Real operational diamond for additive effects. -/
def additiveDiamond :
    DiamondCertificate additiveSystem distinctNatIndependence where
  commute := by
    intro state a b independent
    simp [additive_run, Nat.add_assoc, Nat.add_left_comm, Nat.add_comm]

/-- Nontrivial precondition: total requested increment is at most ten. -/
def boundedPreconditions (word : List Nat) : Prop :=
  word.sum ≤ 10

/-- Nontrivial numeric law: the total increment is even. -/
def evenNumericFlow (word : List Nat) : Prop :=
  Even word.sum

/-- The example's remaining components are explicitly neutral. -/
def additiveSemantics : PlanningSemantics Nat Nat where
  system := additiveSystem
  initial := 0
  goal := fun final => final = 4
  preconditions := boundedPreconditions
  invariants := fun _ => True
  numericFlows := evenNumericFlow
  temporal := fun _ => True
  trajectory := fun _ => True

/-- Bounded preconditions are swap-invariant. -/
theorem boundedPreconditions_swap :
    SwapInvariant distinctNatIndependence boundedPreconditions := by
  intro pre a b suf independent
  unfold boundedPreconditions
  simp [List.sum_append, Nat.add_assoc, Nat.add_left_comm, Nat.add_comm]

/-- Numeric parity is swap-invariant. -/
theorem evenNumericFlow_swap :
    SwapInvariant distinctNatIndependence evenNumericFlow := by
  intro pre a b suf independent
  unfold evenNumericFlow
  simp [List.sum_append, Nat.add_assoc, Nat.add_left_comm, Nat.add_comm]

/-- Neutral predicates are swap-invariant by construction. -/
theorem true_swap_invariant :
    SwapInvariant distinctNatIndependence (fun _ : List Nat => True) := by
  intro pre a b suf independent
  exact Iff.rfl

/-- Complete non-vacuous semantic independence certificate. -/
def additiveCertificate :
    SemanticIndependenceCertificate additiveSemantics distinctNatIndependence where
  effectDiamond := additiveDiamond
  preconditionsStable := boundedPreconditions_swap
  invariantsStable := true_swap_invariant
  numericFlowsStable := evenNumericFlow_swap
  temporalStable := true_swap_invariant
  trajectoryStable := true_swap_invariant

/-- Explicit standing receipt distinguishes active and neutral components. -/
def additiveBoundaryReceipt : SemanticBoundaryReceipt additiveSemantics where
  preconditions := .active [1, 3] (by decide) [11] (by decide)
  invariants := .neutral (fun _ => True.intro)
  numericFlows := .active [1, 3] (by decide) [1, 2] (by decide)
  temporal := .neutral (fun _ => True.intro)
  trajectory := .neutral (fun _ => True.intro)

/-- Anchor execution is lawful. -/
theorem additive_anchor_lawful : additiveSemantics.Lawful [1, 3] := by
  refine ⟨4, ?_, rfl, ?_, trivial, ?_, trivial, trivial⟩
  · decide
  · decide
  · decide

/-- Swapped serialization is trace-equivalent. -/
theorem additive_trace_swap :
    TraceEq distinctNatIndependence [1, 3] [3, 1] := by
  simpa using
    (TraceEq.swap (I := distinctNatIndependence) [] 1 3 [] (by decide))

/-- The original crown obligation is discharged on a concrete non-vacuous
planning semantics. -/
theorem additive_swapped_lawful : additiveSemantics.Lawful [3, 1] :=
  additiveCertificate.traceSwapPreservesLawful
    additive_trace_swap additive_anchor_lawful

end CrownFormal
