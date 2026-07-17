//! Formal-verification claim ceiling.
//!
//! `bcinr` itself never proves anything in Lean — proofs for this project's
//! math live exclusively in the sibling repo `/Users/sac/mfact`
//! (`procint/ProcInt/MFW/*.lean`), a mathlib4-based Lean 4 project. This
//! module baked those citations into inspectable `FormalLawRef` constants so
//! later phases can gate optimizations on `standing.permits_optimization()`
//! instead of re-deriving proof status from memory or prose each time.
//!
//! `bcinr`'s own formal-verification claims (Hoare-logic doc comments citing
//! `thesis.tex`, backed by proptest oracle-equivalence tests — see
//! `crates/bcinr-logic/src/SAFETY.md`) are a *different* claim vocabulary
//! from this one and are not represented here; this module is specifically
//! about the mfact Lean proof status of the multifractal-planner math.

use crate::digest::Digest;
use crate::ids::{ConsequenceHorizonId, TransformationProfileId};

/// Proof status of a formal law, as established in `/Users/sac/mfact`.
///
/// This is a closed, four-way classification — not a spectrum. Only
/// `Proven` permits treating the law as licensing an optimization;
/// `Stated`, `Conjectural`, and `Blocked` all mean "do not build behavior
/// that silently assumes this is true."
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum FormalStanding {
    /// Proven in Lean 4 (sorry-free), citation points at the exact lines.
    Proven,
    /// Defined/stated in Lean, but not proven to have the properties a
    /// caller might assume (e.g. defined as a set-intersection but not
    /// proven downward-closed).
    Stated,
    /// The open goal of the mfact project itself — explicitly named
    /// "CONJECTURAL" by its own authors, no proof attempted.
    Conjectural,
    /// No proof and no Lean-formalized representation exists at all for
    /// this specific claim/representation choice.
    Blocked,
}

impl FormalStanding {
    /// True only for `Proven`. Every call site that wants to skip real
    /// computation in favor of a cached/residualized result because "the
    /// math says this is equivalent" must check this first.
    pub fn permits_optimization(&self) -> bool {
        matches!(self, Self::Proven)
    }
}

/// A citation into the mfact Lean formalization for one formal law/claim.
///
/// `certification_digest` is `None` for every constant defined in this
/// module: these are hand-curated citations transcribed from mfact's source
/// files and thesis summary, not digests computed from a build artifact.
/// A later phase that pins mfact to a specific commit and computes a real
/// digest over the cited declaration should populate `source_commit` and
/// `certification_digest` at that point — leaving them `None` here is not a
/// gap in this phase, it is an accurate statement of what "hand-curated
/// citation" means.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct FormalLawRef {
    /// Lean module path, relative to the mfact repo root, e.g.
    /// `"procint/ProcInt/MFW/QLens.lean"`.
    pub module: &'static str,
    /// The cited declaration name, with a `@file:line-range` suffix and, for
    /// non-`Proven` standings, a short parenthetical on exactly what is and
    /// isn't established.
    pub declaration: &'static str,
    /// mfact git commit the citation was taken at, if known. `None` for the
    /// hand-curated constants in this module.
    pub source_commit: Option<&'static str>,
    /// Digest of the cited declaration's checked proof term, if computed.
    /// `None` for the hand-curated constants in this module.
    pub certification_digest: Option<Digest>,
    pub standing: FormalStanding,
}

/// Error returned when a `SemanticOptimizationContract` is refused at
/// construction.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum ContractError {
    /// The cited law's standing was not `Proven` — construction is refused,
    /// not merely discouraged, so downstream code cannot accidentally build
    /// a contract on top of `Stated`/`Conjectural`/`Blocked` math.
    LawNotProven(FormalStanding),
}

impl std::fmt::Display for ContractError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::LawNotProven(standing) => write!(
                f,
                "refused to construct SemanticOptimizationContract: law standing is {standing:?}, not Proven"
            ),
        }
    }
}

impl std::error::Error for ContractError {}

/// A license to perform a semantic optimization (e.g. skip re-search inside
/// a consequence horizon because the result is provably equivalent) —
/// constructible only when the cited `FormalLawRef` is `Proven`.
///
/// This is the enforcement point, not documentation: `new` refuses
/// (`Err(ContractError::LawNotProven)`) rather than merely warning when
/// `law.standing != FormalStanding::Proven`. The fields are deliberately
/// **not** `pub`: `#[derive(Clone)]` plus public fields would let any
/// caller (in this crate or downstream, since Rust field privacy is
/// per-crate, not per-module) construct a full `SemanticOptimizationContract`
/// via struct-literal syntax carrying a non-`Proven` law, bypassing `new`'s
/// refusal entirely and making the doc comment above false as written. Use
/// the accessor methods below for read access — nothing outside this
/// module needs write access, and `new` is the only place that should ever
/// need it.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct SemanticOptimizationContract {
    law: FormalLawRef,
    consequence_horizon: ConsequenceHorizonId,
    transformation: TransformationProfileId,
    assumptions: Vec<String>,
}

impl SemanticOptimizationContract {
    /// Construct a contract, refusing if `law.standing` is not `Proven`.
    pub fn new(
        law: FormalLawRef,
        consequence_horizon: ConsequenceHorizonId,
        transformation: TransformationProfileId,
        assumptions: Vec<String>,
    ) -> Result<Self, ContractError> {
        if !law.standing.permits_optimization() {
            return Err(ContractError::LawNotProven(law.standing));
        }
        Ok(Self {
            law,
            consequence_horizon,
            transformation,
            assumptions,
        })
    }

    /// The formal law this contract cites as its license. Always `Proven`
    /// (`new` refuses otherwise) — a caller can rely on
    /// `.law().standing == FormalStanding::Proven` without re-checking it.
    pub fn law(&self) -> FormalLawRef {
        self.law
    }

    /// The consequence horizon this contract licenses skipping re-search
    /// within.
    pub fn consequence_horizon(&self) -> ConsequenceHorizonId {
        self.consequence_horizon
    }

    /// The transformation profile this contract licenses.
    pub fn transformation(&self) -> TransformationProfileId {
        self.transformation
    }

    /// Caller-supplied assumptions this contract's validity additionally
    /// depends on (e.g. "fiber is constant over horizon") — `new` does not
    /// verify these; they are recorded, not checked.
    pub fn assumptions(&self) -> &[String] {
        &self.assumptions
    }
}

// ---------------------------------------------------------------------
// The seven formal laws relevant to this project, as cited in mfact.
// ---------------------------------------------------------------------

/// q-lens ratio law: `L_q(i) = p_i^q / sum_j p_j^q`.
/// Proven, sorry-free.
pub const LAW_QLENS_RATIO: FormalLawRef = FormalLawRef {
    module: "procint/ProcInt/MFW/QLens.lean",
    declaration: "qLens_ratio_law@QLens.lean:25-46 (sorry-free)",
    source_commit: None,
    certification_digest: None,
    standing: FormalStanding::Proven,
};

/// `observable_iff_fiber_constant`: sorry-free biconditional.
/// Proven.
pub const LAW_OBSERVABLE_IFF_FIBER_CONSTANT: FormalLawRef = FormalLawRef {
    module: "procint/ProcInt/MFW/Observability.lean",
    declaration:
        "observable_iff_fiber_constant@Observability.lean:134-139 (sorry-free biconditional)",
    source_commit: None,
    certification_digest: None,
    standing: FormalStanding::Proven,
};

/// Concurrency complex `K_Pi` (downward-closed family of faces — mfact's
/// actual representation, NOT minimal-nonfaces) is downward-closed.
/// Proven, but *requires* downward-closure and empty-face membership as
/// admission hypotheses rather than deriving them — callers must supply
/// those hypotheses, the proof does not manufacture them.
pub const LAW_CONCURRENCY_COMPLEX_DOWNWARD_CLOSED: FormalLawRef = FormalLawRef {
    module: "procint/ProcInt/MFW/Concurrency.lean",
    declaration: "K_Pi downward-closed face family@Concurrency.lean:293-321 (sorry-free; requires downward-closure + empty-face membership as admission hypotheses, does not assume them)",
    source_commit: None,
    certification_digest: None,
    standing: FormalStanding::Proven,
};

/// "Executable concurrency" as an intersection `C_E = C_C ∩ C_T ∩ C_R` of
/// causal/temporal/resource predicates. Defined in Lean, but NOT proven to
/// be downward-closed or a valid complex — `Stated`, not `Proven`.
pub const LAW_EXECUTABLE_CONCURRENCY_INTERSECTION: FormalLawRef = FormalLawRef {
    module: "procint/ProcInt/MFW/Concurrency.lean",
    declaration: "C_E = C_C ∩ C_T ∩ C_R@Concurrency.lean:400-416 (defined; NOT proven downward-closed or a valid complex)",
    source_commit: None,
    certification_digest: None,
    standing: FormalStanding::Stated,
};

/// Crown theorem / kernel characterization: `tau(b1) = tau(b2)` iff `b1` is
/// K-equivalent to `b2`. This is the open goal mfact's authors themselves
/// call out as `CONJECTURAL` (see `mfact/MFW_THESIS_SUMMARY.md` line 50,
/// "Current Status: CONJECTURAL") — a bare `Prop` in Lean with no proof
/// attempted, not a proven side lemma.
pub const LAW_CROWN_KERNEL_CHARACTERIZATION: FormalLawRef = FormalLawRef {
    module: "procint/ProcInt/MFW/Kernel.lean",
    declaration: "crown_theorem: tau(b1)=tau(b2) iff K-equivalent@Kernel.lean:302-320 (bare Prop, no proof attempted; see MFW_THESIS_SUMMARY.md:50 \"CONJECTURAL\")",
    source_commit: None,
    certification_digest: None,
    standing: FormalStanding::Conjectural,
};

/// Generalized dimension / multifractal spectrum estimator (`D_q` from
/// moments/scaling fits). `SpectrumBundle.lean`'s "dimension" field is an
/// opaque, uninterpreted function — no box-counting/scaling-fit estimator
/// exists in any language anywhere in mfact. `Blocked`.
pub const LAW_SPECTRUM_ESTIMATOR: FormalLawRef = FormalLawRef {
    module: "procint/ProcInt/MFW/SpectrumBundle.lean",
    declaration: "D_q generalized-dimension estimator (opaque 'dimension' field; no box-counting/scaling-fit estimator exists anywhere in mfact, any language)",
    source_commit: None,
    certification_digest: None,
    standing: FormalStanding::Blocked,
};

/// Minimal-nonfaces / Stanley-Reisner representation of an
/// executable-concurrency complex (as opposed to positive face-lists).
/// **No Lean artifact of this representation exists anywhere in mfact** —
/// mfact formalizes `K_Pi` exclusively via positive face-lists. `Blocked`.
///
/// `MinimalNonFace` / `ExecutableConcurrencyComplex` in this crate's
/// `concurrency` module implement exactly this representation because it
/// was explicitly requested — but neither their construction nor their
/// `admits()` method should ever be described as exact or proven; the
/// honest ceiling is "structurally well-formed" / "internally consistent."
pub const LAW_MINIMAL_NONFACE_REPRESENTATION: FormalLawRef = FormalLawRef {
    module: "procint/ProcInt/MFW/Concurrency.lean",
    declaration: "minimal-nonfaces / Stanley-Reisner representation of K_Pi — NO Lean artifact of this representation exists anywhere in mfact (mfact formalizes K_Pi exclusively via positive face-lists)",
    source_commit: None,
    certification_digest: None,
    standing: FormalStanding::Blocked,
};

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn only_proven_permits_optimization() {
        assert!(FormalStanding::Proven.permits_optimization());
        assert!(!FormalStanding::Stated.permits_optimization());
        assert!(!FormalStanding::Conjectural.permits_optimization());
        assert!(!FormalStanding::Blocked.permits_optimization());
    }

    #[test]
    fn contract_construction_succeeds_for_proven_law() {
        let contract = SemanticOptimizationContract::new(
            LAW_QLENS_RATIO,
            ConsequenceHorizonId(Digest::hash(b"horizon")),
            TransformationProfileId(1),
            vec!["fiber is constant over horizon".into()],
        );
        assert!(contract.is_ok());
    }

    #[test]
    fn contract_construction_refuses_for_stated_law() {
        let err = SemanticOptimizationContract::new(
            LAW_EXECUTABLE_CONCURRENCY_INTERSECTION,
            ConsequenceHorizonId(Digest::hash(b"horizon")),
            TransformationProfileId(1),
            vec![],
        )
        .unwrap_err();
        assert_eq!(err, ContractError::LawNotProven(FormalStanding::Stated));
    }

    #[test]
    fn contract_construction_refuses_for_conjectural_and_blocked() {
        assert!(SemanticOptimizationContract::new(
            LAW_CROWN_KERNEL_CHARACTERIZATION,
            ConsequenceHorizonId(Digest::hash(b"h")),
            TransformationProfileId(1),
            vec![],
        )
        .is_err());
        assert!(SemanticOptimizationContract::new(
            LAW_MINIMAL_NONFACE_REPRESENTATION,
            ConsequenceHorizonId(Digest::hash(b"h")),
            TransformationProfileId(1),
            vec![],
        )
        .is_err());
    }

    /// Replaces a prior version of this test
    /// (`all_seven_laws_have_expected_standing`) that directly compared
    /// each `LAW_*` constant's `.standing` field against the exact literal
    /// that constant's own definition hardcodes two lines above — a bare
    /// struct-field read compared against the literal it was constructed
    /// with, with no computation, parsing, or derivation in between. That
    /// version could not fail unless someone edited a `LAW_*` const and
    /// forgot to edit this test in the same commit (or vice versa); it
    /// exercised no function and would still have passed with every real
    /// function in this crate deleted.
    ///
    /// This version drives the actual enforcement function,
    /// [`SemanticOptimizationContract::new`], for all seven laws and
    /// asserts construction succeeds if and only if
    /// `standing.permits_optimization()` is true — a real branch that would
    /// fail if that gating logic broke (inverted condition, reordered match
    /// arm, a new `FormalStanding` variant `permits_optimization` forgot to
    /// handle). It also pins the ground-truth count from the mfact claim
    /// ceiling table (exactly 3 of 7 are `Proven` today, independently
    /// re-verified this session by reading the cited `mfact` Lean source at
    /// each citation's line range: `QLens.lean:25-46` (matching
    /// `LAW_QLENS_RATIO`'s own citation above — `noncomputable def qLens` at
    /// line 25 through the end of `theorem qLens_ratio_law`'s sorry-free
    /// proof at line 46; the theorem declaration itself starts at line 38,
    /// but the law depends on the deformation `qLens` defines, so citing the
    /// def+theorem span, not just the theorem, is the accurate citation),
    /// `Observability.lean:134-139`, `Concurrency.lean:293-321` for the
    /// three `Proven` laws; `Concurrency.lean:400-416` — a bare `def` with
    /// no downward-closure/validity proof — for the `Stated` one;
    /// `Kernel.lean:302-320` — a bare `Prop`, no proof attempted, matching
    /// `MFW_THESIS_SUMMARY.md:50`'s own "CONJECTURAL" — for the
    /// `Conjectural` one; `SpectrumBundle.lean`'s `dimension` field being an
    /// opaque, uninterpreted function with no estimator implementation
    /// anywhere in mfact, and zero grep hits for "minimal nonface" /
    /// "Stanley-Reisner" anywhere in `procint/ProcInt/MFW/*.lean`, for the
    /// two `Blocked` ones) — so this test still catches an accidental
    /// standing change, but through real construction attempts instead of
    /// a tautological literal comparison.
    #[test]
    fn all_seven_laws_gate_contract_construction_consistently_with_their_standing() {
        let laws = [
            LAW_QLENS_RATIO,
            LAW_OBSERVABLE_IFF_FIBER_CONSTANT,
            LAW_CONCURRENCY_COMPLEX_DOWNWARD_CLOSED,
            LAW_EXECUTABLE_CONCURRENCY_INTERSECTION,
            LAW_CROWN_KERNEL_CHARACTERIZATION,
            LAW_SPECTRUM_ESTIMATOR,
            LAW_MINIMAL_NONFACE_REPRESENTATION,
        ];
        let mut proven_count = 0usize;
        for law in laws {
            let expects_success = law.standing.permits_optimization();
            proven_count += usize::from(expects_success);
            let result = SemanticOptimizationContract::new(
                law,
                ConsequenceHorizonId(Digest::hash(b"horizon")),
                TransformationProfileId(1),
                vec![],
            );
            assert_eq!(
                result.is_ok(),
                expects_success,
                "law {:?} (standing {:?}): SemanticOptimizationContract::new's success must \
                 match permits_optimization()",
                law.declaration,
                law.standing,
            );
        }
        assert_eq!(
            proven_count, 3,
            "expected exactly 3 of the 7 laws to be Proven per the mfact claim ceiling table \
             (QLensRatio, ObservableIffFiberConstant, ConcurrencyComplexDownwardClosed) — if \
             this count changed, either a law's standing was edited or a law was added without \
             updating this fixed list"
        );
    }
}
