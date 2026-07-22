import POWLBridgeVerify

namespace MFW
namespace Crown

universe u

def CrownInvariant {Th : PlanningTheory}
    (S : SemanticIndependence Th)
    {β : Sort u}
    (observable : BehavioralPhaseSpace Th → β) : Prop :=
  ∀ {left right}, LawfulTraceEquiv S left right →
    observable left = observable right

def factorThroughCanonicalTau {Th : PlanningTheory}
    (S : SemanticIndependence Th)
    {β : Sort u}
    (observable : BehavioralPhaseSpace Th → β)
    (invariant : CrownInvariant S observable) :
    CanonicalWorkflowClass S → β :=
  Quotient.lift observable (by
    intro left right h
    exact invariant h)

@[simp] theorem factorThroughCanonicalTau_apply {Th : PlanningTheory}
    (S : SemanticIndependence Th)
    {β : Sort u}
    (observable : BehavioralPhaseSpace Th → β)
    (invariant : CrownInvariant S observable)
    (behavior : BehavioralPhaseSpace Th) :
    factorThroughCanonicalTau S observable invariant (canonicalTau S behavior) =
      observable behavior :=
  rfl

theorem factorThroughCanonicalTau_unique {Th : PlanningTheory}
    (S : SemanticIndependence Th)
    {β : Sort u}
    (observable : BehavioralPhaseSpace Th → β)
    (invariant : CrownInvariant S observable)
    (candidate : CanonicalWorkflowClass S → β)
    (commutes : ∀ behavior,
      candidate (canonicalTau S behavior) = observable behavior) :
    candidate = factorThroughCanonicalTau S observable invariant := by
  funext workflowClass
  refine Quotient.inductionOn workflowClass ?_
  intro behavior
  simpa [canonicalTau, factorThroughCanonicalTau] using commutes behavior

theorem crownInvariant_iff_factors {Th : PlanningTheory}
    (S : SemanticIndependence Th)
    {β : Sort u}
    (observable : BehavioralPhaseSpace Th → β) :
    CrownInvariant S observable ↔
      ∃ workflowObservable : CanonicalWorkflowClass S → β,
        ∀ behavior, observable behavior =
          workflowObservable (canonicalTau S behavior) := by
  constructor
  · intro invariant
    refine ⟨factorThroughCanonicalTau S observable invariant, ?_⟩
    intro behavior
    rfl
  · rintro ⟨workflowObservable, factors⟩
    intro left right hequiv
    calc
      observable left = workflowObservable (canonicalTau S left) := factors left
      _ = workflowObservable (canonicalTau S right) :=
        congrArg workflowObservable (crown_forward S hequiv)
      _ = observable right := (factors right).symm

def canonicalFiber {Th : PlanningTheory}
    (S : SemanticIndependence Th)
    (workflowClass : CanonicalWorkflowClass S) :
    Set (BehavioralPhaseSpace Th) :=
  {behavior | canonicalTau S behavior = workflowClass}

theorem mem_canonicalFiber_iff {Th : PlanningTheory}
    (S : SemanticIndependence Th)
    (center candidate : BehavioralPhaseSpace Th) :
    candidate ∈ canonicalFiber S (canonicalTau S center) ↔
      LawfulTraceEquiv S candidate center := by
  change canonicalTau S candidate = canonicalTau S center ↔ _
  exact crown_kernel_iff S candidate center

theorem canonicalFiber_eq_or_disjoint {Th : PlanningTheory}
    (S : SemanticIndependence Th)
    (left right : CanonicalWorkflowClass S) :
    canonicalFiber S left = canonicalFiber S right ∨
      Disjoint (canonicalFiber S left) (canonicalFiber S right) := by
  by_cases h : left = right
  · exact Or.inl (by simpa [h])
  · right
    rw [Set.disjoint_left]
    intro behavior hleft hright
    exact h (hleft.symm.trans hright)

end Crown
end MFW
