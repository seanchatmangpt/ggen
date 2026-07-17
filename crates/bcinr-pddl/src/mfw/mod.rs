//! MFW q-lens: `L_q(i) = p_i^q / sum_j p_j^q`.
//!
//! # Formal standing
//!
//! This module implements exactly one piece of math this whole project can
//! honestly call formally grounded: the q-lens ratio law,
//! `bcinr_mfw_ir::contracts::LAW_QLENS_RATIO` — `FormalStanding::Proven`,
//! sorry-free, `procint/ProcInt/MFW/QLens.lean:25-46` in `/Users/sac/mfact`.
//! [`q_lens`]'s two proptest properties below
//! (`weights_sum_to_one_within_tolerance`, `ordering_preserved_for_positive_q`)
//! are exactly the two structural facts `qLens_ratio_law` establishes:
//! `sum_i L_q(i) = 1` and, for `q > 0`, `L_q` is order-preserving in `p`.
//! Nothing else in this file, and nothing in [`FrontierMeasure`]/
//! [`MassVector`] below, carries any Lean citation — nowhere here or
//! elsewhere is that implied.
//!
//! # `MassVector` / `FrontierMeasure` — ordinary engineering, not proven
//!
//! `MassVector` and its `project` method are plain, uncited engineering: a
//! six-dimensional description of one search frontier "box" (candidate
//! expansion), collapsed to a single scalar via an equal-weighted sum. No
//! formal law backs this particular weighting or even the choice to
//! linearly combine the six dimensions at all — a later phase is free to
//! replace `project`'s body without touching any formal claim, because there
//! isn't one to preserve.
//!
//! # `planner` submodule — feature-gated, not part of the default build
//!
//! [`planner`] (only compiled with the `mfw-planner` Cargo feature) is the
//! top-level `MfwPlanner` orchestrator wiring this crate's `consequence`/
//! `mfw`/`search`/`causal`/`concurrency` modules together with `bcinr-powl`'s
//! `PowlProjector` and `bcinr-powl-receipt`'s receipt sealing. It is
//! feature-gated, not unconditionally compiled, because this crate's own
//! `Cargo.toml` states a deliberate boundary predating this integration
//! phase: "No path deps on bcinr-powl ... PDDL must not bleed into every
//! consumer. Opt-in only via this crate." Adding `bcinr-powl`/
//! `bcinr-powl-receipt` as unconditional dependencies would silently widen
//! every existing consumer's (including `praxis`'s) transitive dependency
//! graph. `mfw-planner` is off by default; enable it with `--features
//! mfw-planner` (or `cargo test -p bcinr-pddl --features mfw-planner`) to
//! build/exercise the orchestrator.

#[cfg(feature = "mfw-planner")]
pub mod planner;

use std::collections::BTreeMap;

/// Validated exponent for `L_q`. Must be finite — `NaN`/`±Inf` would make
/// `p^q` undefined or degenerate for every `p`, so construction refuses them
/// up front rather than letting a bad `q` propagate into every weight.
#[derive(Debug, Clone, Copy, PartialEq)]
pub struct QValue(f64);

impl QValue {
    pub fn new(q: f64) -> Result<Self, QLensError> {
        if !q.is_finite() {
            return Err(QLensError::NonFiniteQ(q));
        }
        Ok(Self(q))
    }

    pub fn get(self) -> f64 {
        self.0
    }
}

/// Validated positive mass: finite and strictly greater than zero. Used both
/// for [`PositiveDistribution`]'s raw input masses and for
/// [`WeightedDistribution`]'s normalized output weights — a weight that
/// underflowed to exactly `0.0` (extreme `q`, extreme input masses) is
/// refused at construction rather than silently accepted as "positive."
#[derive(Debug, Clone, Copy, PartialEq)]
pub struct PositiveMass(f64);

impl PositiveMass {
    pub fn new(v: f64) -> Result<Self, QLensError> {
        if !v.is_finite() || v <= 0.0 {
            return Err(QLensError::NonPositiveMass(v));
        }
        Ok(Self(v))
    }

    pub fn get(self) -> f64 {
        self.0
    }
}

/// Errors `q_lens` and its supporting constructors can produce. Every
/// variant names a specific degenerate input rather than collapsing to one
/// generic "invalid" case.
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum QLensError {
    /// `q` was `NaN` or `±Inf`.
    NonFiniteQ(f64),
    /// A mass (input or, after normalization, output weight) was zero,
    /// negative, non-finite, or underflowed to exactly zero.
    NonPositiveMass(f64),
    /// `PositiveDistribution`/`q_lens` was given zero entries — `L_q` is
    /// undefined over an empty distribution (there is no `sum_j p_j^q` to
    /// divide by).
    EmptyDistribution,
    /// `sum_j p_j^q` itself was zero, negative, or non-finite (e.g. very
    /// negative `q` combined with a very large mass overflows `p^q` to
    /// `+Inf`, or a very positive `q` combined with a tiny mass underflows
    /// it to `0.0`) — the ratio `p_i^q / sum` is undefined; refused rather
    /// than producing `NaN`/`Inf` weights.
    DegenerateNormalization,
}

impl std::fmt::Display for QLensError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::NonFiniteQ(q) => write!(f, "q-lens exponent is not finite: {q}"),
            Self::NonPositiveMass(v) => write!(f, "mass is not strictly positive/finite: {v}"),
            Self::EmptyDistribution => write!(f, "distribution has no entries"),
            Self::DegenerateNormalization => {
                write!(f, "sum_j p_j^q is zero, negative, or non-finite")
            }
        }
    }
}

impl std::error::Error for QLensError {}

/// An unnormalized distribution of positive masses over keys `K` — the raw
/// input to [`q_lens`].
#[derive(Debug, Clone)]
pub struct PositiveDistribution<K> {
    entries: Vec<(K, PositiveMass)>,
}

impl<K> PositiveDistribution<K> {
    /// Build a distribution from `entries`. Refuses an empty distribution
    /// (nothing to normalize over).
    pub fn new(entries: Vec<(K, PositiveMass)>) -> Result<Self, QLensError> {
        if entries.is_empty() {
            return Err(QLensError::EmptyDistribution);
        }
        Ok(Self { entries })
    }

    pub fn entries(&self) -> &[(K, PositiveMass)] {
        &self.entries
    }
}

/// A normalized `L_q`-weighted distribution over keys `K` — the output of
/// [`q_lens`]. Every weight is a [`PositiveMass`], and the weights sum to
/// `1.0` within float tolerance (see `q_lens`'s doc comment and the
/// `weights_sum_to_one_within_tolerance` proptest).
#[derive(Debug, Clone)]
pub struct WeightedDistribution<K> {
    entries: Vec<(K, PositiveMass)>,
}

impl<K> WeightedDistribution<K> {
    pub fn entries(&self) -> &[(K, PositiveMass)] {
        &self.entries
    }

    /// Sum of all weights — should be `1.0` within float tolerance for any
    /// output `q_lens` actually produced (proven by the ratio law over
    /// reals; this getter lets a caller/test check the floating-point
    /// implementation actually achieves that in practice).
    pub fn sum(&self) -> f64 {
        self.entries.iter().map(|(_, w)| w.get()).sum()
    }
}

/// `L_q(i) = p_i^q / sum_j p_j^q` — real normalization, real error handling
/// for every degenerate case named in [`QLensError`]. See the module doc
/// comment for this function's formal standing
/// (`bcinr_mfw_ir::contracts::LAW_QLENS_RATIO`, `Proven`).
///
/// # Complexity
///
/// O(n) in `distribution.entries().len()` — two linear passes (one
/// `powf` + collect, one normalize), each entry doing one `powf()` call
/// and one division.
pub fn q_lens<K: Clone>(
    q: QValue,
    distribution: &PositiveDistribution<K>,
) -> Result<WeightedDistribution<K>, QLensError> {
    let q_raw = q.get();
    let powered: Vec<(K, f64)> = distribution
        .entries()
        .iter()
        .map(|(k, p)| (k.clone(), p.get().powf(q_raw)))
        .collect();
    let sum: f64 = powered.iter().map(|(_, v)| *v).sum();
    if !sum.is_finite() || sum <= 0.0 {
        return Err(QLensError::DegenerateNormalization);
    }
    let mut out = Vec::with_capacity(powered.len());
    for (k, v) in powered {
        let w = v / sum;
        let mass = PositiveMass::new(w).map_err(|_| QLensError::DegenerateNormalization)?;
        out.push((k, mass));
    }
    Ok(WeightedDistribution { entries: out })
}

// ---------------------------------------------------------------------
// FrontierMeasure / MassVector — ordinary engineering, no Lean backing.
// ---------------------------------------------------------------------

/// Six independent dimensions describing how "promising" one search
/// frontier box (candidate expansion) is. Plain engineering — see the
/// module doc comment.
#[derive(Debug, Clone, Copy, PartialEq)]
pub struct MassVector {
    pub unresolved_goal_mass: f64,
    pub candidate_action_mass: f64,
    pub semantic_novelty_mass: f64,
    pub resource_pressure_mass: f64,
    pub temporal_pressure_mass: f64,
    pub cache_novelty_mass: f64,
}

impl MassVector {
    /// Collapse the six dimensions to a single scalar [`PositiveMass`] via
    /// an equal-weighted sum. No formal justification for this particular
    /// weighting (or even linear combination) exists — see the module doc
    /// comment. Refuses (`QLensError::NonPositiveMass`) if the total is
    /// zero, negative, or non-finite (e.g. every dimension is exactly
    /// zero — a frontier box with genuinely nothing going for it should
    /// not silently receive a positive score).
    pub fn project(&self) -> Result<PositiveMass, QLensError> {
        let total = self.unresolved_goal_mass
            + self.candidate_action_mass
            + self.semantic_novelty_mass
            + self.resource_pressure_mass
            + self.temporal_pressure_mass
            + self.cache_novelty_mass;
        PositiveMass::new(total)
    }
}

/// Produces a [`MassVector`] for one frontier box. Implemented by whatever
/// PDDL-specific type represents a candidate expansion (e.g. a partially
/// expanded state in a portfolio rail) — kept generic here so this module
/// has no dependency on `crate::search`'s rail types.
pub trait FrontierMeasure {
    fn measure(&self) -> MassVector;
}

/// A named collection of frontier boxes and their projected masses — used
/// by `crate::search`'s exploit rail to rank candidates via [`q_lens`].
/// `BTreeMap` (not `HashMap`) for deterministic iteration order, matching
/// this workspace's determinism discipline wherever ordering could feed a
/// digest or a reproducible search trace.
#[derive(Debug, Clone, Default)]
pub struct FrontierBoxes<K: Ord> {
    boxes: BTreeMap<K, MassVector>,
}

impl<K: Ord + Clone> FrontierBoxes<K> {
    pub fn new() -> Self {
        Self {
            boxes: BTreeMap::new(),
        }
    }

    pub fn insert(&mut self, key: K, mass: MassVector) {
        self.boxes.insert(key, mass);
    }

    pub fn is_empty(&self) -> bool {
        self.boxes.is_empty()
    }

    pub fn len(&self) -> usize {
        self.boxes.len()
    }

    /// Project every box's `MassVector` down to a `PositiveMass` and build
    /// a [`PositiveDistribution`] over them — boxes whose projection fails
    /// (a genuinely non-positive total) are skipped, not silently coerced
    /// to some minimum positive value.
    pub fn positive_distribution(&self) -> Result<PositiveDistribution<K>, QLensError> {
        let entries: Vec<(K, PositiveMass)> = self
            .boxes
            .iter()
            .filter_map(|(k, mv)| mv.project().ok().map(|m| (k.clone(), m)))
            .collect();
        PositiveDistribution::new(entries)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use proptest::prelude::*;

    fn mkdist(masses: &[f64]) -> PositiveDistribution<usize> {
        let entries: Vec<(usize, PositiveMass)> = masses
            .iter()
            .enumerate()
            .map(|(i, &m)| (i, PositiveMass::new(m).unwrap()))
            .collect();
        PositiveDistribution::new(entries).unwrap()
    }

    #[test]
    fn q_lens_of_uniform_distribution_is_uniform() {
        let dist = mkdist(&[1.0, 1.0, 1.0, 1.0]);
        let out = q_lens(QValue::new(2.0).unwrap(), &dist).unwrap();
        for (_, w) in out.entries() {
            assert!((w.get() - 0.25).abs() < 1e-12);
        }
    }

    #[test]
    fn q_lens_rejects_empty_distribution() {
        let entries: Vec<(usize, PositiveMass)> = vec![];
        assert_eq!(
            PositiveDistribution::new(entries).unwrap_err(),
            QLensError::EmptyDistribution
        );
    }

    #[test]
    fn q_lens_rejects_non_finite_q() {
        assert!(matches!(
            QValue::new(f64::NAN),
            Err(QLensError::NonFiniteQ(_))
        ));
        assert!(matches!(
            QValue::new(f64::INFINITY),
            Err(QLensError::NonFiniteQ(_))
        ));
    }

    #[test]
    fn positive_mass_rejects_zero_negative_and_non_finite() {
        assert!(PositiveMass::new(0.0).is_err());
        assert!(PositiveMass::new(-1.0).is_err());
        assert!(PositiveMass::new(f64::NAN).is_err());
        assert!(PositiveMass::new(f64::INFINITY).is_err());
        assert!(PositiveMass::new(1.0).is_ok());
    }

    /// Extreme `q` combined with extreme masses genuinely underflows
    /// `sum_j p_j^q` to `0.0` — `q_lens` must refuse, not divide by zero and
    /// propagate `NaN`/`Inf` weights.
    #[test]
    fn q_lens_refuses_degenerate_normalization_on_underflow() {
        let dist = mkdist(&[1e-300, 1e-300]);
        let result = q_lens(QValue::new(50.0).unwrap(), &dist);
        assert_eq!(result.unwrap_err(), QLensError::DegenerateNormalization);
    }

    #[test]
    fn mass_vector_project_sums_all_six_dimensions() {
        let mv = MassVector {
            unresolved_goal_mass: 1.0,
            candidate_action_mass: 2.0,
            semantic_novelty_mass: 3.0,
            resource_pressure_mass: 4.0,
            temporal_pressure_mass: 5.0,
            cache_novelty_mass: 6.0,
        };
        assert_eq!(mv.project().unwrap().get(), 21.0);
    }

    #[test]
    fn mass_vector_project_refuses_all_zero() {
        let mv = MassVector {
            unresolved_goal_mass: 0.0,
            candidate_action_mass: 0.0,
            semantic_novelty_mass: 0.0,
            resource_pressure_mass: 0.0,
            temporal_pressure_mass: 0.0,
            cache_novelty_mass: 0.0,
        };
        assert!(mv.project().is_err());
    }

    #[test]
    fn frontier_boxes_positive_distribution_skips_failed_projections() {
        let mut boxes = FrontierBoxes::new();
        boxes.insert(
            "good",
            MassVector {
                unresolved_goal_mass: 1.0,
                candidate_action_mass: 0.0,
                semantic_novelty_mass: 0.0,
                resource_pressure_mass: 0.0,
                temporal_pressure_mass: 0.0,
                cache_novelty_mass: 0.0,
            },
        );
        boxes.insert(
            "all-zero",
            MassVector {
                unresolved_goal_mass: 0.0,
                candidate_action_mass: 0.0,
                semantic_novelty_mass: 0.0,
                resource_pressure_mass: 0.0,
                temporal_pressure_mass: 0.0,
                cache_novelty_mass: 0.0,
            },
        );
        let dist = boxes.positive_distribution().unwrap();
        assert_eq!(dist.entries().len(), 1);
        assert_eq!(dist.entries()[0].0, "good");
    }

    // ------------------------------------------------------------------
    // Property tests for LAW_QLENS_RATIO's two structural facts.
    // ------------------------------------------------------------------
    proptest! {
        /// `sum_i L_q(i) = 1` — the ratio law's normalization property.
        /// Masses/q kept in a range that never triggers a legitimate
        /// `DegenerateNormalization` refusal (see
        /// `q_lens_refuses_degenerate_normalization_on_underflow` above for
        /// that boundary case tested directly).
        #[test]
        fn weights_sum_to_one_within_tolerance(
            masses in proptest::collection::vec(0.01f64..100.0, 1..12),
            q in 0.1f64..5.0,
        ) {
            let dist = mkdist(&masses);
            let out = q_lens(QValue::new(q).unwrap(), &dist).unwrap();
            prop_assert!((out.sum() - 1.0).abs() < 1e-6);
        }

        /// For `q > 0`, `L_q` preserves the ordering of the input
        /// distribution: `p_i < p_j` implies `L_q(i) < L_q(j)`. This is the
        /// ratio law's order-preservation property.
        #[test]
        fn ordering_preserved_for_positive_q(
            masses in proptest::collection::vec(0.01f64..100.0, 2..12),
            q in 0.1f64..5.0,
        ) {
            let dist = mkdist(&masses);
            let out = q_lens(QValue::new(q).unwrap(), &dist).unwrap();
            let weights: Vec<f64> = out.entries().iter().map(|(_, w)| w.get()).collect();
            for i in 0..masses.len() {
                for j in 0..masses.len() {
                    if masses[i] < masses[j] {
                        prop_assert!(
                            weights[i] < weights[j],
                            "p[{i}]={} < p[{j}]={} but weight[{i}]={} >= weight[{j}]={}",
                            masses[i], masses[j], weights[i], weights[j]
                        );
                    }
                }
            }
        }
    }
}
