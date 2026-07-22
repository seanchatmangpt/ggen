import Mathlib

namespace MFW

structure PlanningTheory where
  State : Type
  Action : Type
  transition : State → Action → Option State
  initial : State
  isGoal : State → Prop
  TemporalConstraint : Type
  temporalConstraints : List TemporalConstraint
  temporalSatisfied : TemporalConstraint → List ℝ → Prop
  NumericFluent : Type
  numericEffect : Action → NumericFluent → ℝ
  numericPrecSatisfied : Action → (NumericFluent → ℝ) → Prop
  TrajectoryConstraint : Type
  trajectoryConstraints : List TrajectoryConstraint
  trajectorySatisfied : TrajectoryConstraint → List State → Prop

structure BehaviorTrace (Th : PlanningTheory) where
  events : List Th.Action
  timestamps : List ℝ
  length_match : events.length = timestamps.length

def BehaviorTrace.stateTrace {Th : PlanningTheory} (b : BehaviorTrace Th) :
    Option (List Th.State) :=
  b.events.foldlM
    (fun acc a => do
      let s ← acc.getLast?
      let s' ← Th.transition s a
      return acc ++ [s'])
    [Th.initial]

def IsLawful (Th : PlanningTheory) (b : BehaviorTrace Th) : Prop :=
  ∃ (trace : List Th.State),
    b.stateTrace = some trace ∧
    trace.length = b.events.length + 1 ∧
    (∃ final ∈ trace.getLast?, Th.isGoal final) ∧
    (∀ tc ∈ Th.temporalConstraints, Th.temporalSatisfied tc b.timestamps) ∧
    (∀ trc ∈ Th.trajectoryConstraints, Th.trajectorySatisfied trc trace)

structure LawfulBehavior (Th : PlanningTheory) where
  trace : BehaviorTrace Th
  lawful : IsLawful Th trace

def BehavioralPhaseSpace (Th : PlanningTheory) := LawfulBehavior Th

inductive Powl (α : Type*) : Type _
  | atom (a : α)
  | silent
  | xor (children : List (Powl α))
  | loop (doP redoP : Powl α)
  | po (children : List (Powl α)) (prec : ℕ → ℕ → Prop)

inductive Powl.WellFormed {α : Type*} : Powl α → Prop
  | atom (a : α) : Powl.WellFormed (Powl.atom a)
  | silent : Powl.WellFormed (Powl.silent : Powl α)
  | xor (children : List (Powl α))
      (hlen : 2 ≤ children.length)
      (hall : ∀ c ∈ children, Powl.WellFormed c) :
      Powl.WellFormed (Powl.xor children)
  | loop (doP redoP : Powl α) :
      Powl.WellFormed doP → Powl.WellFormed redoP →
      Powl.WellFormed (Powl.loop doP redoP)
  | po (children : List (Powl α)) (prec : ℕ → ℕ → Prop)
      (hirr : ∀ i, i < children.length → ¬ prec i i)
      (htrans : ∀ i j k, i < children.length → j < children.length →
        k < children.length → prec i j → prec j k → prec i k)
      (hall : ∀ c ∈ children, Powl.WellFormed c) :
      Powl.WellFormed (Powl.po children prec)

structure ChoiceGraph (α : Type) where
  vertices : List (Powl α)
  edge : Fin vertices.length → Fin vertices.length → Prop
  nontrivial : 2 ≤ vertices.length

structure POWLv2Object (α : Type) where
  model : Powl α
  wellFormed : Powl.WellFormed model
  depth : Nat
  boundaryVars : Finset Nat
  choiceGraphs : List (ChoiceGraph α)

def WorkflowSpace (α : Type) := POWLv2Object α

structure WorkflowTransformation (Th : PlanningTheory) (α : Type) where
  map : BehavioralPhaseSpace Th → WorkflowSpace α

def transformEquiv {Th : PlanningTheory} {α : Type}
    (τ : WorkflowTransformation Th α) :
    BehavioralPhaseSpace Th → BehavioralPhaseSpace Th → Prop :=
  fun b₁ b₂ => τ.map b₁ = τ.map b₂

structure IndependenceRelation (Action : Type) where
  independent : Action → Action → Prop
  symm : ∀ a b, independent a b → independent b a

inductive TraceEquiv {Action : Type} (I : IndependenceRelation Action) :
    List Action → List Action → Prop
  | refl (s : List Action) : TraceEquiv I s s
  | swap (pre : List Action) (a b : Action) (suf : List Action)
      (h : I.independent a b) :
      TraceEquiv I (pre ++ [a, b] ++ suf) (pre ++ [b, a] ++ suf)
  | trans (s₁ s₂ s₃ : List Action) :
      TraceEquiv I s₁ s₂ → TraceEquiv I s₂ s₃ → TraceEquiv I s₁ s₃
  | symm (s₁ s₂ : List Action) :
      TraceEquiv I s₁ s₂ → TraceEquiv I s₂ s₁

theorem traceEquiv_eq_of_no_indep {Action : Type} {I : IndependenceRelation Action}
    (hI : ∀ a b, ¬ I.independent a b) {s t : List Action}
    (h : TraceEquiv I s t) : s = t := by
  induction h with
  | refl _ => rfl
  | swap _ a b _ hab => exact absurd hab (hI a b)
  | trans _ _ _ _ _ ih₁ ih₂ => exact ih₁.trans ih₂
  | symm _ _ _ ih => exact ih.symm

namespace Crown

theorem traceEquiv_length_eq {Action : Type} {I : IndependenceRelation Action}
    {xs ys : List Action} (h : TraceEquiv I xs ys) : xs.length = ys.length := by
  induction h with
  | refl _ => rfl
  | swap pre a b suf _ => simp
  | trans _ _ _ _ _ ih₁ ih₂ => exact ih₁.trans ih₂
  | symm _ _ _ ih => exact ih.symm

def AdjacentBehaviorSwap {Th : PlanningTheory}
    (I : IndependenceRelation Th.Action)
    (left right : BehaviorTrace Th) : Prop :=
  ∃ (pre : List Th.Action) (a b : Th.Action) (suf : List Th.Action),
    I.independent a b ∧
    left.events = pre ++ [a, b] ++ suf ∧
    right.events = pre ++ [b, a] ++ suf ∧
    left.timestamps = right.timestamps

theorem adjacentBehaviorSwap_symm {Th : PlanningTheory}
    {I : IndependenceRelation Th.Action} {left right : BehaviorTrace Th}
    (h : AdjacentBehaviorSwap I left right) :
    AdjacentBehaviorSwap I right left := by
  rcases h with ⟨pre, a, b, suf, hab, hleft, hright, htime⟩
  exact ⟨pre, b, a, suf, I.symm a b hab, hright, hleft, htime.symm⟩

inductive BehaviorSwapEquiv {Th : PlanningTheory}
    (I : IndependenceRelation Th.Action) :
    BehaviorTrace Th → BehaviorTrace Th → Prop
  | refl (b : BehaviorTrace Th) : BehaviorSwapEquiv I b b
  | step {left right : BehaviorTrace Th} :
      AdjacentBehaviorSwap I left right → BehaviorSwapEquiv I left right
  | symm {left right : BehaviorTrace Th} :
      BehaviorSwapEquiv I left right → BehaviorSwapEquiv I right left
  | trans {left middle right : BehaviorTrace Th} :
      BehaviorSwapEquiv I left middle →
      BehaviorSwapEquiv I middle right →
      BehaviorSwapEquiv I left right

namespace BehaviorSwapEquiv

theorem equivalence {Th : PlanningTheory}
    (I : IndependenceRelation Th.Action) : Equivalence (BehaviorSwapEquiv I) :=
  ⟨BehaviorSwapEquiv.refl,
    fun h => BehaviorSwapEquiv.symm h,
    fun h₁ h₂ => BehaviorSwapEquiv.trans h₁ h₂⟩

theorem events {Th : PlanningTheory}
    {I : IndependenceRelation Th.Action}
    {left right : BehaviorTrace Th}
    (h : BehaviorSwapEquiv I left right) :
    TraceEquiv I left.events right.events := by
  induction h with
  | refl b => exact TraceEquiv.refl b.events
  | @step left right hstep =>
      rcases hstep with ⟨pre, a, b, suf, hab, hleft, hright, _⟩
      rw [hleft, hright]
      exact TraceEquiv.swap pre a b suf hab
  | symm h ih => exact TraceEquiv.symm _ _ ih
  | trans h₁ h₂ ih₁ ih₂ => exact TraceEquiv.trans _ _ _ ih₁ ih₂

theorem timestamps {Th : PlanningTheory}
    {I : IndependenceRelation Th.Action}
    {left right : BehaviorTrace Th}
    (h : BehaviorSwapEquiv I left right) :
    left.timestamps = right.timestamps := by
  induction h with
  | refl _ => rfl
  | step hstep =>
      rcases hstep with ⟨_, _, _, _, _, _, _, htime⟩
      exact htime
  | symm _ ih => exact ih.symm
  | trans _ _ ih₁ ih₂ => exact ih₁.trans ih₂

end BehaviorSwapEquiv

def behaviorSwapSetoid {Th : PlanningTheory}
    (I : IndependenceRelation Th.Action) : Setoid (BehaviorTrace Th) where
  r := BehaviorSwapEquiv I
  iseqv := BehaviorSwapEquiv.equivalence I

def runPair (Th : PlanningTheory) (state : Th.State)
    (first second : Th.Action) : Option Th.State := do
  let middle ← Th.transition state first
  Th.transition middle second

structure SemanticIndependence (Th : PlanningTheory) where
  relation : IndependenceRelation Th.Action
  effectsCommute : ∀ {state : Th.State} {a b : Th.Action},
    relation.independent a b → runPair Th state a b = runPair Th state b a
  lawfulSwap : ∀ {left right : BehaviorTrace Th},
    AdjacentBehaviorSwap relation left right →
    (IsLawful Th left ↔ IsLawful Th right)

def SemanticTraceEquiv {Th : PlanningTheory}
    (S : SemanticIndependence Th) :
    BehaviorTrace Th → BehaviorTrace Th → Prop :=
  BehaviorSwapEquiv S.relation

namespace SemanticIndependence

theorem lawful_iff {Th : PlanningTheory} (S : SemanticIndependence Th)
    {left right : BehaviorTrace Th}
    (h : SemanticTraceEquiv S left right) :
    IsLawful Th left ↔ IsLawful Th right := by
  induction h with
  | refl _ => rfl
  | step hstep => exact S.lawfulSwap hstep
  | symm _ ih => exact ih.symm
  | trans _ _ ih₁ ih₂ => exact ih₁.trans ih₂

end SemanticIndependence

def LawfulTraceEquiv {Th : PlanningTheory}
    (S : SemanticIndependence Th)
    (left right : BehavioralPhaseSpace Th) : Prop :=
  SemanticTraceEquiv S left.trace right.trace

theorem lawfulTraceEquiv_equivalence {Th : PlanningTheory}
    (S : SemanticIndependence Th) : Equivalence (LawfulTraceEquiv S) :=
  ⟨fun b => BehaviorSwapEquiv.refl b.trace,
    fun h => BehaviorSwapEquiv.symm h,
    fun h₁ h₂ => BehaviorSwapEquiv.trans h₁ h₂⟩

def lawfulTraceSetoid {Th : PlanningTheory}
    (S : SemanticIndependence Th) : Setoid (BehavioralPhaseSpace Th) where
  r := LawfulTraceEquiv S
  iseqv := lawfulTraceEquiv_equivalence S

def CanonicalWorkflowClass {Th : PlanningTheory}
    (S : SemanticIndependence Th) :=
  Quotient (lawfulTraceSetoid S)

def canonicalTau {Th : PlanningTheory}
    (S : SemanticIndependence Th)
    (behavior : BehavioralPhaseSpace Th) : CanonicalWorkflowClass S :=
  Quotient.mk (lawfulTraceSetoid S) behavior

def CanonicalKernelEquiv {Th : PlanningTheory}
    (S : SemanticIndependence Th)
    (left right : BehavioralPhaseSpace Th) : Prop :=
  canonicalTau S left = canonicalTau S right

theorem crown_forward {Th : PlanningTheory}
    (S : SemanticIndependence Th)
    {left right : BehavioralPhaseSpace Th}
    (h : LawfulTraceEquiv S left right) :
    CanonicalKernelEquiv S left right := by
  exact Quotient.sound h

theorem crown_reverse {Th : PlanningTheory}
    (S : SemanticIndependence Th)
    {left right : BehavioralPhaseSpace Th}
    (h : CanonicalKernelEquiv S left right) :
    LawfulTraceEquiv S left right := by
  exact Quotient.exact h

theorem crown_kernel_iff {Th : PlanningTheory}
    (S : SemanticIndependence Th)
    (left right : BehavioralPhaseSpace Th) :
    CanonicalKernelEquiv S left right ↔ LawfulTraceEquiv S left right := by
  constructor
  · exact crown_reverse S
  · exact crown_forward S

theorem canonicalTau_surjective {Th : PlanningTheory}
    (S : SemanticIndependence Th) : Function.Surjective (canonicalTau S) := by
  intro workflowClass
  refine Quotient.inductionOn workflowClass ?_
  intro behavior
  exact ⟨behavior, rfl⟩

def timestampSensitiveTheory : PlanningTheory where
  State := Bool
  Action := Unit
  transition := fun _ _ => some true
  initial := false
  isGoal := fun state => state = true
  TemporalConstraint := Unit
  temporalConstraints := [()]
  temporalSatisfied := fun _ timestamps => timestamps = [0]
  NumericFluent := Unit
  numericEffect := fun _ _ => 0
  numericPrecSatisfied := fun _ _ => True
  TrajectoryConstraint := Unit
  trajectoryConstraints := []
  trajectorySatisfied := fun _ _ => True

def timestampZeroTrace : BehaviorTrace timestampSensitiveTheory where
  events := [()]
  timestamps := [0]
  length_match := rfl

def timestampOneTrace : BehaviorTrace timestampSensitiveTheory where
  events := [()]
  timestamps := [1]
  length_match := rfl

theorem timestampZeroTrace_lawful :
    IsLawful timestampSensitiveTheory timestampZeroTrace := by
  refine ⟨[false, true], rfl, rfl, ?_, ?_, ?_⟩
  · exact ⟨true, rfl, rfl⟩
  · intro tc _
    cases tc
    rfl
  · intro trc htrc
    simp [timestampSensitiveTheory] at htrc

theorem timestampOneTrace_not_lawful :
    ¬ IsLawful timestampSensitiveTheory timestampOneTrace := by
  rintro ⟨_, _, _, _, htemporal, _⟩
  have h := htemporal () (by simp [timestampSensitiveTheory])
  norm_num [timestampSensitiveTheory, timestampOneTrace] at h

def emptyIndepUnit : IndependenceRelation Unit where
  independent := fun _ _ => False
  symm := by intro _ _ h; exact h

theorem eventOnlyTraceTransport_false :
    ¬ (∀ (I : IndependenceRelation timestampSensitiveTheory.Action)
        (left right : BehaviorTrace timestampSensitiveTheory),
        TraceEquiv I left.events right.events →
        IsLawful timestampSensitiveTheory left →
        IsLawful timestampSensitiveTheory right) := by
  intro claimed
  have bad := claimed emptyIndepUnit timestampZeroTrace timestampOneTrace
    (TraceEquiv.refl [()]) timestampZeroTrace_lawful
  exact timestampOneTrace_not_lawful bad

def twoActionTheory : PlanningTheory where
  State := Bool
  Action := Bool
  transition := fun _ _ => some true
  initial := false
  isGoal := fun state => state = true
  TemporalConstraint := Unit
  temporalConstraints := []
  temporalSatisfied := fun _ _ => True
  NumericFluent := Unit
  numericEffect := fun _ _ => 0
  numericPrecSatisfied := fun _ _ => True
  TrajectoryConstraint := Unit
  trajectoryConstraints := []
  trajectorySatisfied := fun _ _ => True

def oneStepTrace (action : Bool) : BehaviorTrace twoActionTheory where
  events := [action]
  timestamps := [0]
  length_match := rfl

theorem oneStepTrace_lawful (action : Bool) :
    IsLawful twoActionTheory (oneStepTrace action) := by
  refine ⟨[false, true], rfl, rfl, ?_, ?_, ?_⟩
  · exact ⟨true, rfl, rfl⟩
  · intro tc htc
    simp [twoActionTheory] at htc
  · intro trc htrc
    simp [twoActionTheory] at htrc

def lawfulFalseAction : BehavioralPhaseSpace twoActionTheory :=
  ⟨oneStepTrace false, oneStepTrace_lawful false⟩

def lawfulTrueAction : BehavioralPhaseSpace twoActionTheory :=
  ⟨oneStepTrace true, oneStepTrace_lawful true⟩

def emptyIndepBool : IndependenceRelation Bool where
  independent := fun _ _ => False
  symm := by intro _ _ h; exact h

theorem false_true_not_traceEquiv :
    ¬ TraceEquiv emptyIndepBool [false] [true] := by
  intro h
  have heq : ([false] : List Bool) = [true] :=
    traceEquiv_eq_of_no_indep (I := emptyIndepBool)
      (fun _ _ hindependent => hindependent) h
  exact (by decide : ([false] : List Bool) ≠ [true]) heq

def constantWorkflowObject : WorkflowSpace Unit where
  model := Powl.atom ()
  wellFormed := Powl.WellFormed.atom ()
  depth := 0
  boundaryVars := ∅
  choiceGraphs := []

def constantWorkflowTransformation : WorkflowTransformation twoActionTheory Unit where
  map := fun _ => constantWorkflowObject

theorem arbitraryTransformationKernel_false :
    ¬ (∀ left right : BehavioralPhaseSpace twoActionTheory,
        transformEquiv constantWorkflowTransformation left right ↔
          TraceEquiv emptyIndepBool left.trace.events right.trace.events) := by
  intro claimed
  have hkernel : transformEquiv constantWorkflowTransformation
      lawfulFalseAction lawfulTrueAction := rfl
  have htrace := (claimed lawfulFalseAction lawfulTrueAction).mp hkernel
  exact false_true_not_traceEquiv htrace

end Crown
end MFW
