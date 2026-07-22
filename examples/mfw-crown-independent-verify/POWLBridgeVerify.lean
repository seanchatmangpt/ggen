import CrownVerify

namespace MFW
namespace Crown

structure FaithfulPOWLRealization {Th : PlanningTheory}
    (S : SemanticIndependence Th) (α : Type) where
  encode : CanonicalWorkflowClass S → WorkflowSpace α
  injective : Function.Injective encode

def realizedTau {Th : PlanningTheory} {α : Type}
    (S : SemanticIndependence Th)
    (R : FaithfulPOWLRealization S α) : WorkflowTransformation Th α where
  map := fun behavior => R.encode (canonicalTau S behavior)

theorem realizedTau_forward {Th : PlanningTheory} {α : Type}
    (S : SemanticIndependence Th)
    (R : FaithfulPOWLRealization S α)
    {left right : BehavioralPhaseSpace Th}
    (h : LawfulTraceEquiv S left right) :
    transformEquiv (realizedTau S R) left right := by
  change R.encode (canonicalTau S left) = R.encode (canonicalTau S right)
  exact congrArg R.encode (crown_forward S h)

theorem realizedTau_reverse {Th : PlanningTheory} {α : Type}
    (S : SemanticIndependence Th)
    (R : FaithfulPOWLRealization S α)
    {left right : BehavioralPhaseSpace Th}
    (h : transformEquiv (realizedTau S R) left right) :
    LawfulTraceEquiv S left right := by
  change R.encode (canonicalTau S left) = R.encode (canonicalTau S right) at h
  exact crown_reverse S (R.injective h)

theorem realizedTau_kernel_iff {Th : PlanningTheory} {α : Type}
    (S : SemanticIndependence Th)
    (R : FaithfulPOWLRealization S α)
    (left right : BehavioralPhaseSpace Th) :
    transformEquiv (realizedTau S R) left right ↔
      LawfulTraceEquiv S left right := by
  constructor
  · exact realizedTau_reverse S R
  · exact realizedTau_forward S R

theorem injective_of_reverse_crown {Th : PlanningTheory} {α : Type}
    (S : SemanticIndependence Th)
    (encode : CanonicalWorkflowClass S → WorkflowSpace α)
    (reverse : ∀ left right : BehavioralPhaseSpace Th,
      encode (canonicalTau S left) = encode (canonicalTau S right) →
        LawfulTraceEquiv S left right) :
    Function.Injective encode := by
  intro x y hxy
  rcases canonicalTau_surjective S x with ⟨left, hleft⟩
  rcases canonicalTau_surjective S y with ⟨right, hright⟩
  have hencoded :
      encode (canonicalTau S left) = encode (canonicalTau S right) := by
    rw [hleft, hright]
    exact hxy
  have hcanonical : canonicalTau S left = canonicalTau S right :=
    crown_forward S (reverse left right hencoded)
  exact hleft.symm.trans (hcanonical.trans hright)

end Crown
end MFW
