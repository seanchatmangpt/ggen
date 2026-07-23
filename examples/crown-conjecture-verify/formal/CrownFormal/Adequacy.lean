import CrownFormal.Examples

set_option autoImplicit false

/-!
# Adequacy boundary and observation-factored behavioral crown

The abstract trace theorem is valid, but it cannot by itself discharge a
behavioral theorem whose lawfulness predicate observes data not carried by the
trace relation. This module makes that boundary kernel-visible.

Two countermodels are decisive:

1. event-only trace equivalence does not relate timestamps, so two behaviors
   with identical events can disagree on temporal lawfulness;
2. equality of final replay states does not imply equality of intermediate
   state traces, so arbitrary trajectory predicates need a separate stability
   certificate.

The repaired interface does not assume pairwise lawfulness preservation.
Instead, native lawfulness must factor through the canonical trace class and an
explicit admitted observation. A related behavior pair must preserve both
coordinates. The behavioral theorem is then derived by rewriting those
coordinates, so the trace kernel is load-bearing rather than decorative.
-/

namespace CrownFormal

universe u v w

/-- Timed behavior with timestamps keyed by event identity rather than by list
position. `uniqueEvents` makes event labels valid occurrence identifiers on the
finite behavior. Reordering events therefore does not silently reassign their
timestamps. -/
structure TimedBehavior (Action : Type u) where
  events : List Action
  timestamps : Action → Nat
  uniqueEvents : events.Nodup

/-- A temporal law observing the timestamp attached to one event identity. -/
def TimestampLawful {Action : Type u} (anchor : Action)
    (behavior : TimedBehavior Action) : Prop :=
  behavior.timestamps anchor = 0

private def goodBoolTimestamps : Bool → Nat
  | false => 0
  | true => 1

private def badBoolTimestamps : Bool → Nat
  | false => 1
  | true => 1

/-- Lawful timed behavior. -/
def timedGood : TimedBehavior Bool where
  events := [false]
  timestamps := goodBoolTimestamps
  uniqueEvents := by decide

/-- Same event word, but an unlawful timestamp observation. -/
def timedBad : TimedBehavior Bool where
  events := [false]
  timestamps := badBoolTimestamps
  uniqueEvents := by decide

/-- The two behaviors are indistinguishable to event-only trace equivalence. -/
theorem timed_events_trace_eq :
    TraceEq distinctBoolIndependence timedGood.events timedBad.events := by
  exact TraceEq.refl [false]

/-- Event-only trace equivalence cannot preserve arbitrary timed lawfulness.
This directly falsifies any theorem quantified over two independent behavior
records while relating only their event lists. -/
theorem event_only_lawfulness_not_preserved :
    ¬ (∀ left right : TimedBehavior Bool,
        TraceEq distinctBoolIndependence left.events right.events →
        TimestampLawful false left → TimestampLawful false right) := by
  intro claimed
  have transferred : TimestampLawful false timedBad :=
    claimed timedGood timedBad timed_events_trace_eq (by rfl)
  have refused : ¬ TimestampLawful false timedBad := by decide
  exact refused transferred

/-- A sufficient timed relation: event words are trace-equivalent and the
identity-keyed timestamp observation is equal. Unlike positional timestamp-list
equality, this relation lets a timestamp travel with its event when an admitted
commutation changes serialization order. -/
def TimedTraceEq {Action : Type u} (I : Independence Action)
    (left right : TimedBehavior Action) : Prop :=
  TraceEq I left.events right.events ∧
  left.timestamps = right.timestamps

/-- `TimedTraceEq` is an equivalence relation. -/
theorem timedTraceEq_equivalence {Action : Type u} (I : Independence Action) :
    Equivalence (TimedTraceEq I) :=
  ⟨
    fun behavior => ⟨TraceEq.refl behavior.events, rfl⟩,
    fun related => ⟨TraceEq.symm related.1, related.2.symm⟩,
    fun first second =>
      ⟨TraceEq.trans first.1 second.1, first.2.trans second.2⟩
  ⟩

/-- The complete timed observation closes the timestamp countermodel. -/
theorem timestampLawful_preserved_by_timedTraceEq
    {Action : Type u} {I : Independence Action} {anchor : Action}
    {left right : TimedBehavior Action}
    (equivalent : TimedTraceEq I left right) :
    TimestampLawful anchor left ↔ TimestampLawful anchor right := by
  unfold TimestampLawful
  rw [equivalent.2]

/-- Replay retaining the complete state trajectory, including the initial
state. This is intentionally distinct from final-state replay. -/
def TransitionSystem.runStateTrace
    {State : Type u} {Action : Type v}
    (system : TransitionSystem State Action) :
    State → List Action → Option (List State)
  | state, [] => some [state]
  | state, action :: rest => do
      let next ← system.step state action
      let tail ← system.runStateTrace next rest
      pure (state :: tail)

/-- The additive diamond has the same final state under swapping but different
intermediate trajectories. -/
theorem additive_same_final_different_state_trace :
    additiveSystem.run 0 [1, 3] = additiveSystem.run 0 [3, 1] ∧
    additiveSystem.runStateTrace 0 [1, 3] = some [0, 1, 4] ∧
    additiveSystem.runStateTrace 0 [3, 1] = some [0, 3, 4] := by
  decide

/-- A trajectory observation distinguishes the two state traces even though
the final-state diamond holds. -/
theorem trajectory_observation_counterexample :
    (1 ∈ ([0, 1, 4] : List Nat)) ∧
    ¬ (1 ∈ ([0, 3, 4] : List Nat)) := by
  decide

/-- A trajectory-aware relation must expose the state-trace observation rather
than infer it from final-state equality. -/
def AdditiveTrajectoryRelated (left right : List Nat) : Prop :=
  TraceEq distinctNatIndependence left right ∧
  additiveSystem.runStateTrace 0 left =
    additiveSystem.runStateTrace 0 right

/-- The additive swap is correctly refused by the complete trajectory relation.
It is trace-equivalent at the event layer, but its observed state traces differ. -/
theorem additive_trajectory_relation_refuses_swap :
    ¬ AdditiveTrajectoryRelated [1, 3] [3, 1] := by
  intro related
  have different :
      additiveSystem.runStateTrace 0 [1, 3] ≠
        additiveSystem.runStateTrace 0 [3, 1] := by
    decide
  exact different related.2

/-- Behavioral adequacy boundary.

`lawfulFactorization` is pointwise, not the desired pairwise theorem: it states
that native lawfulness is represented by a predicate over the canonical trace
class and one explicit observation. `traceSound` and `observationEq` then make
both inputs equal for related behaviors. -/
structure CrownObservationAdmission
    {Behavior : Type u} {Action : Type v} {Observation : Type w}
    (I : Independence Action)
    (events : Behavior → List Action)
    (observe : Behavior → Observation)
    (lawful : Behavior → Prop) where
  related : Behavior → Behavior → Prop
  relationEquivalence : Equivalence related
  traceSound : ∀ {left right},
    related left right → TraceEq I (events left) (events right)
  observationEq : ∀ {left right},
    related left right → observe left = observe right
  admittedLaw : TraceClass I → Observation → Prop
  lawfulFactorization : ∀ behavior,
    lawful behavior ↔
      admittedLaw (classify I (events behavior)) (observe behavior)

namespace CrownObservationAdmission

variable {Behavior : Type u} {Action : Type v} {Observation : Type w}
variable {I : Independence Action}
variable {events : Behavior → List Action}
variable {observe : Behavior → Observation}
variable {lawful : Behavior → Prop}

/-- Related behaviors have the same native lawfulness because both admitted
semantic coordinates are equal. The proof necessarily consumes `traceSound`,
`classify_eq_iff`, `observationEq`, and `lawfulFactorization`. -/
theorem lawfulIff
    (admission : CrownObservationAdmission I events observe lawful)
    {left right : Behavior}
    (related : admission.related left right) :
    lawful left ↔ lawful right := by
  rw [admission.lawfulFactorization left,
    admission.lawfulFactorization right]
  have traceClassEq :
      classify I (events left) = classify I (events right) :=
    (classify_eq_iff I (events left) (events right)).2
      (admission.traceSound related)
  have observationEq : observe left = observe right :=
    admission.observationEq related
  rw [traceClassEq, observationEq]

/-- Directional behavioral preservation after complete observation admission. -/
theorem preservesLawful
    (admission : CrownObservationAdmission I events observe lawful)
    {left right : Behavior}
    (related : admission.related left right)
    (leftLawful : lawful left) :
    lawful right :=
  (admission.lawfulIff related).mp leftLawful

end CrownObservationAdmission

/-- Non-vacuous behavioral crown evidence. The witness pair must be related but
must have genuinely different event serializations. Together with `traceSound`,
this proves that the admitted relation contains at least one nontrivial trace
commutation rather than only reflexive or impossible pairs. -/
structure AdmittedBehavioralCrown
    {Behavior : Type u} {Action : Type v} {Observation : Type w}
    (I : Independence Action)
    (events : Behavior → List Action)
    (observe : Behavior → Observation)
    (lawful : Behavior → Prop) where
  admission : CrownObservationAdmission I events observe lawful
  witnessLeft : Behavior
  witnessRight : Behavior
  witnessRelated : admission.related witnessLeft witnessRight
  distinctEvents : events witnessLeft ≠ events witnessRight

namespace AdmittedBehavioralCrown

variable {Behavior : Type u} {Action : Type v} {Observation : Type w}
variable {I : Independence Action}
variable {events : Behavior → List Action}
variable {observe : Behavior → Observation}
variable {lawful : Behavior → Prop}

/-- Admitted bidirectional lawfulness preservation. -/
theorem lawfulIff
    (crown : AdmittedBehavioralCrown I events observe lawful)
    {left right : Behavior}
    (related : crown.admission.related left right) :
    lawful left ↔ lawful right :=
  crown.admission.lawfulIff related

/-- Admitted directional lawfulness preservation. -/
theorem preservesLawful
    (crown : AdmittedBehavioralCrown I events observe lawful)
    {left right : Behavior}
    (related : crown.admission.related left right)
    (leftLawful : lawful left) :
    lawful right :=
  (crown.lawfulIff related).mp leftLawful

end AdmittedBehavioralCrown

/-- A nontrivial timed serialization with identity-keyed timestamps. -/
def timedSwapLeft : TimedBehavior Bool where
  events := [false, true]
  timestamps := goodBoolTimestamps
  uniqueEvents := by decide

/-- The same timed occurrences in the opposite admitted serialization. -/
def timedSwapRight : TimedBehavior Bool where
  events := [true, false]
  timestamps := goodBoolTimestamps
  uniqueEvents := by decide

/-- The timed relation contains a genuine adjacent commutation and keeps the
same event-to-timestamp assignment. -/
theorem timed_nontrivial_related :
    TimedTraceEq distinctBoolIndependence timedSwapLeft timedSwapRight := by
  have independent : distinctBoolIndependence.independent false true := by
    change false ≠ true
    decide
  refine ⟨?_, rfl⟩
  simpa using
    (TraceEq.swap (I := distinctBoolIndependence) [] false true [] independent)

/-- Observation-factored timed admission. Native lawfulness is represented by a
predicate over the trace class and the full identity-keyed timestamp map. -/
def timedObservationAdmission :
    CrownObservationAdmission distinctBoolIndependence
      (fun behavior : TimedBehavior Bool => behavior.events)
      (fun behavior : TimedBehavior Bool => behavior.timestamps)
      (TimestampLawful false) where
  related := TimedTraceEq distinctBoolIndependence
  relationEquivalence := timedTraceEq_equivalence distinctBoolIndependence
  traceSound := fun related => related.1
  observationEq := fun related => related.2
  admittedLaw := fun _ timestamps => timestamps false = 0
  lawfulFactorization := fun _ => Iff.rfl

/-- The behavioral crown cannot be inhabited only by reflexivity: it carries a
concrete swapped timed pair with distinct serializations. -/
def timedAdmittedCrown :
    AdmittedBehavioralCrown distinctBoolIndependence
      (fun behavior : TimedBehavior Bool => behavior.events)
      (fun behavior : TimedBehavior Bool => behavior.timestamps)
      (TimestampLawful false) where
  admission := timedObservationAdmission
  witnessLeft := timedSwapLeft
  witnessRight := timedSwapRight
  witnessRelated := timed_nontrivial_related
  distinctEvents := by decide

/-- The repaired theorem transports timed lawfulness by deriving it from trace
classification and observation equality rather than assuming pairwise
lawfulness equivalence. -/
theorem timed_repaired_crown
    {left right : TimedBehavior Bool}
    (related : timedAdmittedCrown.admission.related left right)
    (lawful : TimestampLawful false left) :
    TimestampLawful false right :=
  timedAdmittedCrown.preservesLawful related lawful

/-- The nontrivial swapped timed example is lawful on both serializations. -/
theorem timed_swapped_lawful : TimestampLawful false timedSwapRight := by
  apply timed_repaired_crown timed_nontrivial_related
  rfl

end CrownFormal