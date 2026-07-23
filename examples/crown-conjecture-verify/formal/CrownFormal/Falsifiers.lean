import CrownFormal.Crown

set_option autoImplicit false

/-!
# Falsifiers and non-vacuity witnesses

These examples pin the theorem boundary. They prove that an arbitrary
transformation does not have the crown and that an order-sensitive transition
system cannot manufacture a false diamond certificate.
-/

namespace CrownFormal

/-- No actions commute. -/
def emptyBoolIndependence : Independence Bool where
  independent := fun _ _ => False
  symmetric := by intro a b h; exact h
  irreflexive := by intro a h; exact h

/-- Under empty independence, trace equivalence collapses to word equality. -/
theorem traceEq_eq_of_empty {left right : List Bool}
    (equivalent : TraceEq emptyBoolIndependence left right) :
    left = right := by
  induction equivalent with
  | refl word => rfl
  | swap pre a b suf independent => exact False.elim independent
  | symm h ih => exact ih.symm
  | trans h₁ h₂ ih₁ ih₂ => exact ih₁.trans ih₂

/-- Constant maps collapse distinct trace classes, refuting the unrestricted
arbitrary-transformation version of the crown. -/
theorem constant_map_not_crown :
    ¬ CrownLaw emptyBoolIndependence (fun _ : List Bool => Unit.unit) := by
  intro crown
  have equivalent :
      TraceEq emptyBoolIndependence [false] [true] :=
    (crown [false] [true]).mp rfl
  have equalWords : [false] = [true] := traceEq_eq_of_empty equivalent
  exact (by decide : ([false] : List Bool) ≠ [true]) equalWords

/-- Distinct Boolean actions are independent. -/
def distinctBoolIndependence : Independence Bool where
  independent := fun a b => a ≠ b
  symmetric := fun h => Ne.symm h
  irreflexive := fun _ h => h rfl

/-- Order-sensitive transition system: `false` writes `false`; `true` toggles. -/
def orderSensitiveSystem : TransitionSystem Bool Bool where
  step := fun state action =>
    some (if action then !state else false)

/-- The two independent actions produce different final states in opposite
orders. -/
theorem orderSensitive_replays_differ :
    orderSensitiveSystem.run true [false, true] ≠
      orderSensitiveSystem.run true [true, false] := by
  decide

/-- Therefore no operational diamond certificate exists for the claimed
independence relation. -/
theorem no_false_diamond :
    ¬ DiamondCertificate orderSensitiveSystem distinctBoolIndependence := by
  intro certificate
  have distinct : distinctBoolIndependence.independent false true := by
    change false ≠ true
    decide
  have commute := certificate.commute true false true distinct
  exact orderSensitive_replays_differ commute

end CrownFormal
