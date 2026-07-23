import CrownFormal.Examples

set_option autoImplicit false

/-!
# Adequacy boundary and repaired behavioral crown

The abstract trace theorem is valid, but it cannot by itself discharge a
behavioral theorem whose lawfulness predicate observes data not carried by the
trace relation. This module makes that boundary kernel-visible.

Two countermodels are decisive:

1. event-only trace equivalence does not relate timestamps, so two behaviors
   with identical events can disagree on temporal lawfulness;
2. equality of final replay states does not imply equality of intermediate
   state traces, so arbitrary trajectory predicates need a separate stability
   certificate.

The repaired interface is `CrownObservationAdmission`: downstream semantics
must supply an admitted behavioral relation that both projects to event trace
equivalence and preserves the complete native lawfulness predicate.
-/

namespace CrownFormal

universe u v

/-- Minimal timed behavior exposing the observation omitted by an event-only
trace relation. -/
structure TimedBehavior (Action : Type u) where
  events : List Action
  timestamps : List Nat
  length_match : events.length = timestamps.length

/-- A deliberately order-observable temporal law. It is small enough to serve
as a closed countermodel while retaining the exact defect: lawfulness reads
timestamps that event trace equivalence does not constrain. -/
def TimestampLawful {Action : Type u} (behavior : TimedBehavior Action) : Prop :=
  behavior.timestamps = [0]

/-- Lawful timed behavior. -/
def timedGood : TimedBehavior Bool where
  events := [false]
  timestamps := [0]
  length_match := rfl

/-- Same events and length, but an unlawful temporal observation. -/
def timedBad : TimedBehavior Bool where
  events := [false]
  timestamps := [1]
  length_match := rfl

/-- The two behaviors are indistinguishable to event-only trace equivalence. -/
theorem timed_events_trace_eq :
    TraceEq emptyBoolIndependence timedGood.events timedBad.events := by
  exact TraceEq.refl [false]

/-- Event-only trace equivalence cannot preserve arbitrary timed lawfulness.
This is a direct falsifier of any theorem quantified over two independent
behavior records while relating only their event lists. -/
theorem event_only_lawfulness_not_preserved :
    ¬ (∀ left right : TimedBehavior Bool,
        TraceEq emptyBoolIndependence left.events right.events →
        TimestampLawful left → TimestampLawful right) := by
  intro claimed
  have transferred : TimestampLawful timedBad :=
    claimed timedGood timedBad timed_events_trace_eq (by rfl)
  have refused : ¬ TimestampLawful timedBad := by
    unfold TimestampLawful
    decide
  exact refused transferred

/-- A sufficient repaired relation for the timed countermodel: events are
trace-equivalent and the observation that temporal lawfulness reads is aligned.
A concrete PDDL adapter may replace equality with a richer schedule isomorphism,
but it must expose and prove that morphism explicitly. -/
def TimedTraceEq {Action : Type u} (I : Independence Action)
    (left right : TimedBehavior Action) : Prop :=
  TraceEq I left.events right.events ∧
  left.timestamps = right.timestamps

/-- The added observation relation closes the timestamp countermodel. -/
theorem timestampLawful_preserved_by_timedTraceEq
    {Action : Type u} {I : Independence Action}
    {left right : TimedBehavior Action}
    (equivalent : TimedTraceEq I left right) :
    TimestampLawful left ↔ TimestampLawful right := by
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

/-- Behavioral admission boundary for applying an event-trace theorem to a
native behavior type. `related` is the admitted observation relation. Its two
laws prevent an event-only proof from being silently promoted to a theorem
about richer behaviors. -/
structure CrownObservationAdmission
    {Behavior : Type u} {Action : Type v}
    (I : Independence Action)
    (events : Behavior → List Action)
    (lawful : Behavior → Prop) where
  related : Behavior → Behavior → Prop
  traceSound : ∀ {left right},
    related left right → TraceEq I (events left) (events right)
  lawfulIff : ∀ {left right},
    related left right → (lawful left ↔ lawful right)

namespace CrownObservationAdmission

variable {Behavior : Type u} {Action : Type v}
variable {I : Independence Action}
variable {events : Behavior → List Action}
variable {lawful : Behavior → Prop}

/-- Directional behavioral preservation after complete observation admission. -/
theorem preservesLawful
    (admission : CrownObservationAdmission I events lawful)
    {left right : Behavior}
    (related : admission.related left right)
    (leftLawful : lawful left) :
    lawful right :=
  (admission.lawfulIff related).mp leftLawful

end CrownObservationAdmission

/-- Concrete repaired admission for the timed countermodel. -/
def timedObservationAdmission :
    CrownObservationAdmission emptyBoolIndependence
      (fun behavior : TimedBehavior Bool => behavior.events)
      TimestampLawful where
  related := TimedTraceEq emptyBoolIndependence
  traceSound := fun related => related.1
  lawfulIff := fun related =>
    timestampLawful_preserved_by_timedTraceEq related

/-- The repaired theorem now transports timed lawfulness through the admitted
observation relation. -/
theorem timed_repaired_crown
    {left right : TimedBehavior Bool}
    (related : timedObservationAdmission.related left right)
    (lawful : TimestampLawful left) :
    TimestampLawful right :=
  timedObservationAdmission.preservesLawful related lawful

end CrownFormal
