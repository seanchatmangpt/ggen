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
  irreflexive := fun _ h => h rfl

/-- Additive transition semantics. -/
def additiveSystem : TransitionSystem Nat Nat where
  step := fun state action => some (state + action)

/-- Replay is addition of the word sum. -/
theorem additive_run (state : Nat) (word : List Nat) :
    additiveSystem.run state word = some (state + word.sum) := by
  induction word generalizing state with
  | nil => rfl
  | cons action rest ih =>
      calc
        additiveSystem.run state (action :: rest) =
            additiveSystem.run (state + action) rest := rfl
        _ = some ((state + action) + rest.sum) := ih (state + action)
        _ = some (state + (action + rest.sum)) := by
          rw [Nat.add_assoc]

/-- Real operational diamond for additive effects. -/
def additiveDiamond :
    DiamondCertificate additiveSystem distinctNatIndependence where
  commute := by
    intro state a b _independent
    change some ((state + a) + b) = some ((state + b) + a)
    congr 1
    omega

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
  intro pre a b suf _independent
  unfold boundedPreconditions
  simp only [List.sum_append, List.sum_cons, List.sum_nil, Nat.add_zero]
  omega

/-- Numeric parity is swap-invariant. -/
theorem evenNumericFlow_swap :
    SwapInvariant distinctNatIndependence evenNumericFlow := by
  intro pre a b suf _independent
  unfold evenNumericFlow
  simp only [List.sum_append, List.sum_cons, List.sum_nil, Nat.add_zero]
  constructor <;> rintro ⟨k, hk⟩
  · refine ⟨k, ?_⟩
    omega
  · refine ⟨k, ?_⟩
    omega

/-- Neutral predicates are swap-invariant by construction. -/
theorem true_swap_invariant :
    SwapInvariant distinctNatIndependence (fun _ : List Nat => True) := by
  intro pre a b suf _independent
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
  preconditions := .active [1, 3]
    (by
      change boundedPreconditions [1, 3]
      decide)
    [11]
    (by
      change ¬ boundedPreconditions [11]
      decide)
  invariants := .neutral (fun _ => True.intro)
  numericFlows := .active [1, 3]
    (by
      change evenNumericFlow [1, 3]
      exact ⟨2, rfl⟩)
    [1, 2]
    (by
      change ¬ evenNumericFlow [1, 2]
      rintro ⟨k, hk⟩
      omega)
  temporal := .neutral (fun _ => True.intro)
  trajectory := .neutral (fun _ => True.intro)

/-- Anchor execution is lawful. -/
theorem additive_anchor_lawful : additiveSemantics.Lawful [1, 3] := by
  refine ⟨4, ?_, rfl, ?_, trivial, ?_, trivial, trivial⟩
  · change additiveSystem.run 0 [1, 3] = some 4
    decide
  · change boundedPreconditions [1, 3]
    decide
  · change evenNumericFlow [1, 3]
    exact ⟨2, rfl⟩

/-- Swapped serialization is trace-equivalent. -/
theorem additive_trace_swap :
    TraceEq distinctNatIndependence [1, 3] [3, 1] := by
  have distinct : distinctNatIndependence.independent 1 3 := by
    change 1 ≠ 3
    decide
  simpa using
    (TraceEq.swap (I := distinctNatIndependence) [] 1 3 [] distinct)

/-- The original crown obligation is discharged on a concrete non-vacuous
planning semantics. -/
theorem additive_swapped_lawful : additiveSemantics.Lawful [3, 1] :=
  additiveCertificate.traceSwapPreservesLawful
    additive_trace_swap additive_anchor_lawful

end CrownFormal
