import CrownFormal.Semantics

set_option autoImplicit false

/-!
# Exact kernel characterization

The crown is true unconditionally for the canonical trace quotient. For an
arbitrary transformation it is equivalent to two independent obligations:
soundness with respect to trace equivalence and reflection of trace classes.
-/

namespace CrownFormal

universe u v

/-- Kernel relation of a transformation. -/
def KernelEquiv {X : Type u} {Y : Type v} (transform : X → Y)
    (left right : X) : Prop :=
  transform left = transform right

/-- Exact crown law. -/
def CrownLaw {Action : Type u} {Output : Type v}
    (I : Independence Action) (transform : List Action → Output) : Prop :=
  ∀ left right,
    KernelEquiv transform left right ↔ TraceEq I left right

/-- Forward obligation: accidental serialization is erased. -/
def TraceSound {Action : Type u} {Output : Type v}
    (I : Independence Action) (transform : List Action → Output) : Prop :=
  ∀ {left right}, TraceEq I left right → transform left = transform right

/-- Reverse obligation: distinct trace classes are not collapsed. -/
def TraceReflecting {Action : Type u} {Output : Type v}
    (I : Independence Action) (transform : List Action → Output) : Prop :=
  ∀ {left right}, transform left = transform right → TraceEq I left right

/-- The exact crown decomposes into soundness and reflection. -/
theorem crownLaw_iff_sound_and_reflecting
    {Action : Type u} {Output : Type v}
    (I : Independence Action) (transform : List Action → Output) :
    CrownLaw I transform ↔
      TraceSound I transform ∧ TraceReflecting I transform := by
  constructor
  · intro crown
    exact ⟨
      fun equivalent => (crown _ _).mpr equivalent,
      fun equal => (crown _ _).mp equal
    ⟩
  · rintro ⟨sound, reflecting⟩ left right
    exact ⟨reflecting, sound⟩

/-- Canonical quotient classifier satisfies the crown without extra
hypotheses. -/
theorem quotient_crown {Action : Type u} (I : Independence Action) :
    CrownLaw I (classify I) := by
  intro left right
  exact classify_eq_iff I left right

/-- Complete trace-language semantics also has exactly the trace kernel. -/
theorem traceLanguage_crown {Action : Type u} (I : Independence Action) :
    CrownLaw I (traceLanguage I) := by
  intro left right
  exact traceLanguage_eq_iff I left right

/-- Observable properties are exactly those constant on trace classes. -/
def TraceInvariant {Action : Type u} {Value : Type v}
    (I : Independence Action) (observable : List Action → Value) : Prop :=
  ∀ {left right}, TraceEq I left right → observable left = observable right

/-- Any function factored through the canonical classifier is trace-invariant. -/
theorem invariant_of_factorization
    {Action : Type u} {Value : Type v}
    (I : Independence Action)
    (factor : TraceClass I → Value) :
    TraceInvariant I (fun word => factor (classify I word)) := by
  intro left right equivalent
  exact congrArg factor ((classify_eq_iff I left right).mpr equivalent)

/-- A compiler into any semantic object receives the crown once its semantic
map is both trace-sound and trace-reflecting. The theorem does not hide those
two algorithm-correctness obligations inside the output type. -/
structure CrownCompiler {Action : Type u} (I : Independence Action)
    (Output : Type v) where
  compile : List Action → Output
  sound : TraceSound I compile
  reflecting : TraceReflecting I compile

/-- Every admitted compiler certificate yields the exact kernel theorem. -/
theorem CrownCompiler.crown
    {Action : Type u} {Output : Type v} {I : Independence Action}
    (compiler : CrownCompiler I Output) :
    CrownLaw I compiler.compile :=
  (crownLaw_iff_sound_and_reflecting I compiler.compile).mpr
    ⟨compiler.sound, compiler.reflecting⟩

end CrownFormal
