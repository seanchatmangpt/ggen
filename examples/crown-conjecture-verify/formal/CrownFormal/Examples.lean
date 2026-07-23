import CrownFormal.Falsifiers

set_option autoImplicit false

/-!
# Constructive admitted model

A concrete arithmetic system demonstrates that the certificate and admission
interfaces are inhabited by executable, non-vacuous semantics. Every semantic
layer is active: each carries both an accepting and a rejecting witness.
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

/-- Active precondition: total requested increment is at most ten. -/
def boundedPreconditions (word : List Nat) : Prop :=
  word.sum ≤ 10

/-- Active invariant: the requested increment is nonzero. -/
def nonzeroInvariant (word : List Nat) : Prop :=
  word.sum ≠ 0

/-- Active numeric law: the total increment is even. -/
def evenNumericFlow (word : List Nat) : Prop :=
  Even word.sum

/-- Active abstract temporal law: the aggregate demand is below eight. -/
def boundedTemporal (word : List Nat) : Prop :=
  word.sum < 8

/-- Active abstract trajectory law: aggregate demand avoids six. -/
def avoidsSixTrajectory (word : List Nat) : Prop :=
  word.sum ≠ 6

/-- Fully active abstract semantics. The temporal and trajectory predicates are
not claimed to be a PDDL adapter; `Adequacy.lean` states that boundary exactly. -/
def additiveSemantics : PlanningSemantics Nat Nat where
  system := additiveSystem
  initial := 0
  goal := fun final => final = 4
  preconditions := boundedPreconditions
  invariants := nonzeroInvariant
  numericFlows := evenNumericFlow
  temporal := boundedTemporal
  trajectory := avoidsSixTrajectory

/-- Bounded preconditions are swap-invariant. -/
theorem boundedPreconditions_swap :
    SwapInvariant distinctNatIndependence boundedPreconditions := by
  intro pre a b suf _independent
  unfold boundedPreconditions
  simp only [List.sum_append, List.sum_cons, List.sum_nil, Nat.add_zero]
  omega

/-- The nonzero invariant is swap-invariant. -/
theorem nonzeroInvariant_swap :
    SwapInvariant distinctNatIndependence nonzeroInvariant := by
  intro pre a b suf _independent
  unfold nonzeroInvariant
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

/-- The abstract temporal bound is swap-invariant. -/
theorem boundedTemporal_swap :
    SwapInvariant distinctNatIndependence boundedTemporal := by
  intro pre a b suf _independent
  unfold boundedTemporal
  simp only [List.sum_append, List.sum_cons, List.sum_nil, Nat.add_zero]
  omega

/-- The abstract trajectory exclusion is swap-invariant. -/
theorem avoidsSixTrajectory_swap :
    SwapInvariant distinctNatIndependence avoidsSixTrajectory := by
  intro pre a b suf _independent
  unfold avoidsSixTrajectory
  simp only [List.sum_append, List.sum_cons, List.sum_nil, Nat.add_zero]
  omega

/-- Complete preservation certificate. -/
def additiveCertificate :
    SemanticIndependenceCertificate additiveSemantics distinctNatIndependence where
  effectDiamond := additiveDiamond
  preconditionsStable := boundedPreconditions_swap
  invariantsStable := nonzeroInvariant_swap
  numericFlowsStable := evenNumericFlow_swap
  temporalStable := boundedTemporal_swap
  trajectoryStable := avoidsSixTrajectory_swap

/-- Explicit standing receipt: every semantic component is active. -/
def additiveBoundaryReceipt : SemanticBoundaryReceipt additiveSemantics where
  goal := .active 4 rfl 5 (by decide)
  preconditions := .active [1, 3]
    (by
      change (4 : Nat) ≤ 10
      decide)
    [11]
    (by
      change ¬(11 : Nat) ≤ 10
      decide)
  invariants := .active [1, 3]
    (by
      change (4 : Nat) ≠ 0
      decide)
    []
    (by
      change ¬((0 : Nat) ≠ 0)
      decide)
  numericFlows := .active [1, 3]
    (by
      change Even (4 : Nat)
      decide)
    [1, 2]
    (by
      change ¬Even (3 : Nat)
      decide)
  temporal := .active [1, 3]
    (by
      change (4 : Nat) < 8
      decide)
    [8]
    (by
      change ¬((8 : Nat) < 8)
      decide)
  trajectory := .active [1, 3]
    (by
      change (4 : Nat) ≠ 6
      decide)
    [6]
    (by
      change ¬((6 : Nat) ≠ 6)
      decide)

/-- Concrete successful independent pair for the operational admission rail. -/
def additiveOperationalReceipt :
    OperationalBoundaryReceipt additiveSemantics distinctNatIndependence where
  state := 0
  leftAction := 1
  rightAction := 3
  independent := by
    change 1 ≠ 3
    decide
  final := 4
  leftReplay := by decide
  rightReplay := by decide

/-- The admitted crown bundles preservation with semantic and operational
standing. -/
def additiveAdmittedCrown :
    AdmittedSemanticCrown additiveSemantics distinctNatIndependence where
  preservation := additiveCertificate
  boundary := additiveBoundaryReceipt
  operational := additiveOperationalReceipt

/-- Anchor execution is lawful. -/
theorem additive_anchor_lawful : additiveSemantics.Lawful [1, 3] := by
  refine ⟨4, ?_, rfl, ?_, ?_, ?_, ?_, ?_⟩
  · change additiveSystem.run 0 [1, 3] = some 4
    decide
  · change (4 : Nat) ≤ 10
    decide
  · change (4 : Nat) ≠ 0
    decide
  · change Even (4 : Nat)
    decide
  · change (4 : Nat) < 8
    decide
  · change (4 : Nat) ≠ 6
    decide

/-- Swapped serialization is trace-equivalent. -/
theorem additive_trace_swap :
    TraceEq distinctNatIndependence [1, 3] [3, 1] := by
  have distinct : distinctNatIndependence.independent 1 3 := by
    change 1 ≠ 3
    decide
  simpa using
    (TraceEq.swap (I := distinctNatIndependence) [] 1 3 [] distinct)

/-- The repaired abstract crown is discharged through the admitted interface,
not through a disconnected preservation proof alone. -/
theorem additive_swapped_lawful : additiveSemantics.Lawful [3, 1] :=
  additiveAdmittedCrown.traceSwapPreservesLawful
    additive_trace_swap additive_anchor_lawful

/-- The proof-relevant execution receipt also transports to the swapped word. -/
theorem additive_swapped_execution :
    TransitionSystem.Execution additiveSystem 0 [3, 1] 4 := by
  have original : TransitionSystem.Execution additiveSystem 0 [1, 3] 4 :=
    TransitionSystem.Execution.of_run_eq (by decide)
  exact additiveDiamond.transport_execution additive_trace_swap original

end CrownFormal
