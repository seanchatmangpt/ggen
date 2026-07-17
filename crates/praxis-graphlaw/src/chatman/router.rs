//! Least-expressive dialect routing for the chatman engine.
//!
//! The routing law: a query is always answered by the **least expressive**
//! dialect that can express it and that the active profile permits. The
//! [`Dialect`] enum's derived `Ord` *is* that law — variants are declared in
//! ascending expressive power, so `a < b` means "`a` is strictly less
//! expressive than `b`". Any component claiming a more expressive route than
//! [`DialectRouter::decide`] would choose is refused with
//! [`Refusal::LeastExpressiveRouteViolation`].
//!
//! All hashing goes through [`wasm4pm_compat::hash::blake3_combined`] with
//! version- and field-tagged material; no wall clock, no randomness.

use serde::{Deserialize, Serialize};
use wasm4pm_compat::hash::blake3_combined;

use super::abi::{Digest, ProfileId, Refusal};

/// Domain-tag prefix for [`ProfileGates::hash`] material (versioned so a
/// future scheme change cannot collide with v1 digests).
const PROFILE_HASH_TAG: &str = "chatman/router/profile-gates/v1";

/// Domain-tag prefix for [`RouteDecision`] `decision_hash` material.
const DECISION_HASH_TAG: &str = "chatman/router/route-decision/v1";

/// Query dialects ordered **ascending by expressive power**.
///
/// The declaration order (and therefore the derived `Ord`) is the
/// least-expressive-route law: `Triple8Pattern < ShaclCore < SparqlSelect <
/// SparqlConstruct < OwlRl < N3`. Do not reorder variants — the router,
/// the mask bits, and the LER verification all depend on this order.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize)]
#[repr(u8)]
pub enum Dialect {
    /// Fixed-arity 8-constraint triple pattern matching (hot path).
    Triple8Pattern = 0,
    /// SHACL Core shape validation.
    ShaclCore = 1,
    /// SPARQL SELECT over the snapshot.
    SparqlSelect = 2,
    /// SPARQL CONSTRUCT (graph-producing queries).
    SparqlConstruct = 3,
    /// OWL RL materialization.
    OwlRl = 4,
    /// N3 rules with builtins (cold path; never actuation-capable).
    N3 = 5,
}

impl Dialect {
    /// Every dialect in ascending expressive order. Iteration over this array
    /// is the canonical least-expressive-first scan.
    pub const ALL: [Dialect; 6] = [
        Dialect::Triple8Pattern,
        Dialect::ShaclCore,
        Dialect::SparqlSelect,
        Dialect::SparqlConstruct,
        Dialect::OwlRl,
        Dialect::N3,
    ];

    /// Maps a dialect to its execution route. `Triple8Pattern` is the only
    /// hot dialect; `N3` is the only cold dialect; everything between is warm.
    ///
    /// # Complexity
    /// O(1) — exhaustive match on a fieldless enum.
    pub const fn route(self) -> Route {
        match self {
            Dialect::Triple8Pattern => Route::Hot,
            Dialect::ShaclCore
            | Dialect::SparqlSelect
            | Dialect::SparqlConstruct
            | Dialect::OwlRl => Route::Warm,
            Dialect::N3 => Route::Cold,
        }
    }

    /// The bit this dialect occupies in a profile mask (`1 << discriminant`).
    ///
    /// # Complexity
    /// O(1).
    pub const fn mask_bit(self) -> u8 {
        1 << (self as u8)
    }

    /// Stable name used as field-tagged hash material.
    ///
    /// # Complexity
    /// O(1).
    pub const fn name(self) -> &'static str {
        match self {
            Dialect::Triple8Pattern => "Triple8Pattern",
            Dialect::ShaclCore => "ShaclCore",
            Dialect::SparqlSelect => "SparqlSelect",
            Dialect::SparqlConstruct => "SparqlConstruct",
            Dialect::OwlRl => "OwlRl",
            Dialect::N3 => "N3",
        }
    }
}

/// Execution route tiers, ordered ascending by cost.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize)]
pub enum Route {
    /// Sub-microsecond fixed-pattern path (`Triple8Pattern` only).
    Hot,
    /// Bounded query/validation path.
    Warm,
    /// Unbounded-rule reasoning path (`N3` only).
    Cold,
}

impl Route {
    /// Stable name used as field-tagged hash material.
    ///
    /// # Complexity
    /// O(1).
    pub const fn name(self) -> &'static str {
        match self {
            Route::Hot => "Hot",
            Route::Warm => "Warm",
            Route::Cold => "Cold",
        }
    }
}

/// Per-profile dialect permissions. Constructed only through
/// [`ProfileGates::new`], which enforces the gate laws.
#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct ProfileGates {
    /// Identity of the profile these gates belong to.
    pub profile_id: ProfileId,
    /// Bitmask of dialects the profile may execute at all. The N3 bit is
    /// **default 0** (see [`ProfileGates::DEFAULT_ENABLED_MASK`]); enabling
    /// N3 is always an explicit act.
    pub enabled_dialects_mask: u8,
    /// Bitmask of dialects permitted to drive actuation (side effects).
    /// Must be a subset of `enabled_dialects_mask` and must never contain N3.
    pub actuation_dialects_mask: u8,
    /// Maximum constraint count the hot path accepts (≤ 8 by construction).
    pub max_hot_constraints: u8,
}

impl ProfileGates {
    /// Default enabled mask: every dialect except N3. N3 stays off unless a
    /// profile turns it on deliberately.
    pub const DEFAULT_ENABLED_MASK: u8 = Dialect::Triple8Pattern.mask_bit()
        | Dialect::ShaclCore.mask_bit()
        | Dialect::SparqlSelect.mask_bit()
        | Dialect::SparqlConstruct.mask_bit()
        | Dialect::OwlRl.mask_bit();

    /// Builds validated gates.
    ///
    /// # Errors
    /// - [`Refusal::N3ActuationRefused`] if the actuation mask contains the
    ///   N3 bit — N3 may never actuate, regardless of enablement.
    /// - [`Refusal::ValidationFailed`] if the actuation mask is not a subset
    ///   of the enabled mask, or if `max_hot_constraints > 8`.
    ///
    /// # Complexity
    /// O(1) — bit arithmetic only.
    pub fn new(
        profile_id: ProfileId,
        enabled_dialects_mask: u8,
        actuation_dialects_mask: u8,
        max_hot_constraints: u8,
    ) -> Result<Self, Refusal> {
        if actuation_dialects_mask & Dialect::N3.mask_bit() != 0 {
            return Err(Refusal::N3ActuationRefused(format!(
                "profile {profile_id}: actuation mask {actuation_dialects_mask:#010b} \
                 contains the N3 bit; N3 may never drive actuation"
            )));
        }
        if actuation_dialects_mask & !enabled_dialects_mask != 0 {
            return Err(Refusal::ValidationFailed(format!(
                "profile {profile_id}: actuation mask {actuation_dialects_mask:#010b} is not \
                 a subset of enabled mask {enabled_dialects_mask:#010b}"
            )));
        }
        if max_hot_constraints > 8 {
            return Err(Refusal::ValidationFailed(format!(
                "profile {profile_id}: max_hot_constraints {max_hot_constraints} exceeds the \
                 hot-path ceiling of 8"
            )));
        }
        Ok(Self {
            profile_id,
            enabled_dialects_mask,
            actuation_dialects_mask,
            max_hot_constraints,
        })
    }

    /// Whether the profile enables `dialect` at all.
    ///
    /// # Complexity
    /// O(1).
    pub const fn is_enabled(&self, dialect: Dialect) -> bool {
        self.enabled_dialects_mask & dialect.mask_bit() != 0
    }

    /// Whether the profile permits `dialect` to drive actuation.
    ///
    /// # Complexity
    /// O(1).
    pub const fn permits_actuation(&self, dialect: Dialect) -> bool {
        self.actuation_dialects_mask & dialect.mask_bit() != 0
    }

    /// Field-tagged BLAKE3 digest of the gates. Same gates → byte-identical
    /// digest; any field change → different digest. Hashing is delegated to
    /// [`blake3_combined`] (length-prefixed, injective); no wall clock.
    ///
    /// # Complexity
    /// O(|profile_id|) — dominated by hashing the identity string.
    pub fn hash(&self) -> Digest {
        let enabled = self.enabled_dialects_mask.to_string();
        let actuation = self.actuation_dialects_mask.to_string();
        let max_hot = self.max_hot_constraints.to_string();
        Digest::new(blake3_combined(&[
            PROFILE_HASH_TAG,
            "profile_id",
            self.profile_id.as_str(),
            "enabled_dialects_mask",
            &enabled,
            "actuation_dialects_mask",
            &actuation,
            "max_hot_constraints",
            &max_hot,
        ]))
    }
}

/// The expressiveness demands of one query, as classified by the caller.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct QueryShape {
    /// Number of triple constraints in the query.
    pub constraint_count: u8,
    /// Whether the query produces a graph (CONSTRUCT semantics).
    pub requires_construct: bool,
    /// Whether the query needs OWL RL entailment.
    pub requires_owl: bool,
    /// Whether the query needs N3 builtins.
    pub requires_n3_builtins: bool,
    /// Whether the query intends to drive actuation (side effects).
    pub wants_actuation: bool,
}

impl QueryShape {
    /// The least expressive dialect capable of expressing this shape,
    /// ignoring profile gates and the hot-path constraint budget.
    /// Capabilities are monotone in `Dialect`'s order: every dialect ≥ the
    /// returned floor can also express the shape.
    ///
    /// # Complexity
    /// O(1).
    const fn minimum_dialect(&self) -> Dialect {
        if self.requires_n3_builtins {
            Dialect::N3
        } else if self.requires_owl {
            Dialect::OwlRl
        } else if self.requires_construct {
            Dialect::SparqlConstruct
        } else {
            Dialect::Triple8Pattern
        }
    }

    /// Field-tagged hash material for this shape (bools encoded as 0/1;
    /// fixed field order and separators, so the encoding is injective over
    /// `QueryShape`).
    ///
    /// # Complexity
    /// O(1) — five bounded fields.
    fn hash_material(&self) -> String {
        format!(
            "constraint_count={};construct={};owl={};n3={};actuation={}",
            self.constraint_count,
            self.requires_construct as u8,
            self.requires_owl as u8,
            self.requires_n3_builtins as u8,
            self.wants_actuation as u8
        )
    }
}

/// The router's binding answer for one query shape under one profile.
///
/// Derives only what upstream [`Digest`] supports (no serde: `Digest` in
/// `wasm4pm_compat` does not derive `Serialize`/`Deserialize`).
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct RouteDecision {
    /// The least expressive permitted dialect.
    pub dialect: Dialect,
    /// The execution route implied by `dialect`.
    pub route: Route,
    /// Digest of the [`ProfileGates`] the decision was made under.
    pub profile_hash: Digest,
    /// Digest binding (profile, shape, dialect, route) together.
    pub decision_hash: Digest,
}

/// Routes query shapes to the least expressive permitted dialect under a
/// fixed [`ProfileGates`].
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct DialectRouter {
    gates: ProfileGates,
}

impl DialectRouter {
    /// Builds a router over validated gates.
    pub fn new(gates: ProfileGates) -> Self {
        Self { gates }
    }

    /// Borrows the gates this router decides under.
    pub fn gates(&self) -> &ProfileGates {
        &self.gates
    }

    /// Decides the least expressive permitted dialect for `shape`.
    ///
    /// Scans [`Dialect::ALL`] ascending from the shape's capability floor and
    /// returns the first dialect that is (a) capable, (b) enabled by the
    /// profile, (c) within the hot constraint budget when hot, and (d)
    /// actuation-permitted when the shape wants actuation.
    ///
    /// # Errors
    /// - [`Refusal::N3UnavailableByProfile`] — the shape needs N3 builtins
    ///   but the profile does not enable N3.
    /// - [`Refusal::N3ActuationRefused`] — the shape needs N3 builtins *and*
    ///   wants actuation; N3 never actuates.
    /// - [`Refusal::WarmPathRequired`] — the shape exceeds the hot constraint
    ///   budget, so a warm dialect is required, but the profile enables none
    ///   that qualifies.
    /// - [`Refusal::UnsupportedDialect`] — no enabled dialect satisfies the
    ///   shape for any other reason.
    ///
    /// # Complexity
    /// O(1) — the scan covers the fixed 6-variant [`Dialect::ALL`]; each
    /// candidate check is bit arithmetic. Sealing the decision hashes the
    /// profile identity, O(|profile_id|).
    pub fn decide(&self, shape: &QueryShape) -> Result<RouteDecision, Refusal> {
        let floor = shape.minimum_dialect();

        if floor == Dialect::N3 {
            if !self.gates.is_enabled(Dialect::N3) {
                return Err(Refusal::N3UnavailableByProfile(format!(
                    "profile {}: shape requires N3 builtins but the profile does not \
                     enable N3 (enabled mask {:#010b})",
                    self.gates.profile_id, self.gates.enabled_dialects_mask
                )));
            }
            if shape.wants_actuation {
                return Err(Refusal::N3ActuationRefused(format!(
                    "profile {}: shape requires N3 builtins and wants actuation; \
                     N3 may never drive actuation",
                    self.gates.profile_id
                )));
            }
        }

        // Whether the shape was hot-capable except for the constraint budget:
        // distinguishes WarmPathRequired from UnsupportedDialect on miss.
        let mut hot_blocked_by_budget = false;

        // O(1): fixed 6-iteration scan, ascending expressive power (LER law).
        for dialect in Dialect::ALL {
            if dialect < floor {
                continue;
            }
            if dialect == Dialect::Triple8Pattern
                && shape.constraint_count > self.gates.max_hot_constraints
            {
                hot_blocked_by_budget = self.gates.is_enabled(Dialect::Triple8Pattern);
                continue;
            }
            if !self.gates.is_enabled(dialect) {
                continue;
            }
            if shape.wants_actuation && !self.gates.permits_actuation(dialect) {
                if dialect == Dialect::N3 {
                    // N3 can never be in the actuation mask (gate law), so an
                    // actuation-wanting shape landing here is a hard refusal.
                    return Err(Refusal::N3ActuationRefused(format!(
                        "profile {}: only N3 could express the shape but N3 may never \
                         drive actuation",
                        self.gates.profile_id
                    )));
                }
                continue;
            }
            return Ok(self.seal(shape, dialect));
        }

        if hot_blocked_by_budget {
            return Err(Refusal::WarmPathRequired(format!(
                "profile {}: shape has {} constraints, exceeding the hot budget of {}; \
                 a warm path is required but no enabled warm dialect qualifies",
                self.gates.profile_id, shape.constraint_count, self.gates.max_hot_constraints
            )));
        }
        Err(Refusal::UnsupportedDialect(format!(
            "profile {}: no enabled dialect satisfies the shape (enabled mask {:#010b}, \
             actuation mask {:#010b}, wants_actuation={})",
            self.gates.profile_id,
            self.gates.enabled_dialects_mask,
            self.gates.actuation_dialects_mask,
            shape.wants_actuation
        )))
    }

    /// Verifies a claimed decision against the router's own decision.
    ///
    /// # Errors
    /// - [`Refusal::LeastExpressiveRouteViolation`] — the claim names a
    ///   dialect strictly more expressive than the router would choose.
    /// - [`Refusal::RouteDecisionMismatch`] — any other drift (dialect,
    ///   route, profile hash, or decision hash differs).
    /// - Any refusal [`DialectRouter::decide`] itself returns for the shape.
    ///
    /// # Complexity
    /// O(1) — one `decide` plus constant-size comparisons.
    pub fn verify_claim(&self, shape: &QueryShape, claimed: &RouteDecision) -> Result<(), Refusal> {
        let computed = self.decide(shape)?;
        if claimed.dialect > computed.dialect {
            return Err(Refusal::LeastExpressiveRouteViolation(format!(
                "profile {}: claimed dialect {} is more expressive than the least \
                 expressive permitted dialect {}",
                self.gates.profile_id,
                claimed.dialect.name(),
                computed.dialect.name()
            )));
        }
        if claimed != &computed {
            return Err(Refusal::RouteDecisionMismatch(format!(
                "profile {}: claimed decision drifts from computed decision \
                 (claimed dialect {} route {} decision_hash {}; computed dialect {} \
                 route {} decision_hash {})",
                self.gates.profile_id,
                claimed.dialect.name(),
                claimed.route.name(),
                claimed.decision_hash.0,
                computed.dialect.name(),
                computed.route.name(),
                computed.decision_hash.0
            )));
        }
        Ok(())
    }

    /// Builds the sealed decision for a chosen dialect. Material is version-
    /// and field-tagged; same (gates, shape, dialect) → byte-identical digest.
    ///
    /// # Complexity
    /// O(|profile_id|) — dominated by the profile-identity hash.
    fn seal(&self, shape: &QueryShape, dialect: Dialect) -> RouteDecision {
        let route = dialect.route();
        let profile_hash = self.gates.hash();
        let shape_material = shape.hash_material();
        let decision_hash = Digest::new(blake3_combined(&[
            DECISION_HASH_TAG,
            "profile_hash",
            &profile_hash.0,
            "shape",
            &shape_material,
            "dialect",
            dialect.name(),
            "route",
            route.name(),
        ]));
        RouteDecision {
            dialect,
            route,
            profile_hash,
            decision_hash,
        }
    }
}

// ---------------------------------------------------------------------------
// N3 controlled execution surface (PROJ-779, extended by PROJ-780).
//
// PRD §12 ("N3 Quarantine") names five requirements for N3 execution:
// explicit profile capability, declared cost bounds, a builtin whitelist, a
// controlled execution surface, and receipt/replay support (plus, separately,
// zero direct actuation: "An N3 rule that requests direct actuation SHALL be
// refused", `PRD.md:608`). `ProfileGates`/`DialectRouter::decide` above
// already enforce the first requirement (N3 defaults off; enabling it is an
// explicit act — PROJ-777/778). The types below are the halves PROJ-779
// added — a real, enforced cost bound and a real builtin whitelist, both
// checked by [`N3Executor::run`] rather than merely declared and ignored —
// plus [`N3ActuationBuiltin`]/[`N3Rule::direct_actuation_builtins`], PROJ-780's
// addition: PROJ-779's [`N3Builtin`] is a closed, *pure*-only vocabulary (no
// I/O/network/dispatch variant exists in it, by construction), so a rule
// requesting direct actuation was previously simply inexpressible rather
// than actively checked and refused. [`N3ActuationBuiltin`] gives the
// controlled execution surface a way to represent such a request (as
// caller-declared classification data, matching how `builtins`/
// `declared_cost` are caller-declared rather than parsed — `N3Executor`
// still does not parse N3 syntax), and [`N3Executor::run`] refuses it
// unconditionally via [`Refusal::N3DirectActuationRefused`]. The full
// typed-refusal-catalog wiring (this crate's `ALL_REFUSAL_NAMES`, the
// acceptance schemas) for these three N3 variants landed in the same
// catalog-completeness pass that closed PROJ-786/787's other 12 deferred
// variants; see `abi.rs`'s `ALL_REFUSAL_NAMES` doc comment.
// ---------------------------------------------------------------------------

/// Domain-tag prefix for [`N3ExecutionReceipt::execution_hash`] material.
const N3_EXECUTION_HASH_TAG: &str = "chatman/router/n3-execution/v1";

/// Abstract N3 execution cost, in ticks — the same declared-not-measured
/// unit convention as `crates/praxis-synthesis/src/budget.rs`'s `Ticks` /
/// `TickBudget` / `CHATMAN_CONSTANT`: one tick is one declared unit of
/// bounded work, never a measured CPU cycle or a wall-clock duration.
/// Reimplemented locally rather than taken as a cross-crate dependency
/// (this ticket's scope is `router.rs` only, and `praxis-graphlaw` does not
/// otherwise depend on `praxis-synthesis`), with intentionally identical
/// semantics — saturating accumulation, strict `used > limit` exhaustion —
/// so a future cross-crate consolidation is a pure dedup, never a behavior
/// change.
#[derive(
    Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize, Default,
)]
pub struct N3Ticks(pub u64);

/// Declared cost ceiling for one N3 execution, with branchless accounting
/// mirroring `TickBudget::consume` (`crates/praxis-synthesis/src/budget.rs`):
/// saturating add, then strict `used > limit` decides exhaustion (spending
/// exactly the limit is still within budget).
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub struct N3CostBound {
    /// Maximum allowed cumulative ticks for the whole execution.
    pub limit: N3Ticks,
    /// Ticks consumed so far.
    pub used: N3Ticks,
}

impl N3CostBound {
    /// Builds a fresh, unconsumed bound at `limit`.
    ///
    /// # Complexity
    /// O(1).
    pub const fn new(limit: N3Ticks) -> Self {
        Self {
            limit,
            used: N3Ticks(0),
        }
    }

    /// Whether the bound is already spent (`used >= limit`).
    ///
    /// # Complexity
    /// O(1).
    pub const fn is_exhausted(&self) -> bool {
        self.used.0 >= self.limit.0
    }

    /// Consumes `ticks`, saturating rather than wrapping, and reports
    /// whether cumulative usage is still within budget after this
    /// consumption (`false` means this consumption exhausted the bound).
    ///
    /// # Complexity
    /// O(1).
    pub fn consume(&mut self, ticks: N3Ticks) -> bool {
        self.used = N3Ticks(self.used.0.saturating_add(ticks.0));
        self.used.0 <= self.limit.0
    }
}

/// N3 builtin predicates this engine may whitelist for execution, drawn from
/// the pure, side-effect-free subset of the `log:`/`math:`/`string:`/`list:`
/// SWAP builtin vocabularies (<https://www.w3.org/2000/10/swap/>). Builtins
/// capable of I/O, network, or process actuation (e.g. `log:webOperation`)
/// are deliberately never modeled here — this whitelist can only ever grant
/// access to a builtin drawn from this fixed, pure set, regardless of what
/// mask value a profile declares.
///
/// Eight variants by construction, so the whitelist fits the same
/// bit-per-variant mask convention as [`Dialect::mask_bit`] in a `u8`.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize)]
#[repr(u8)]
pub enum N3Builtin {
    /// `log:equalTo` — RDF term identity.
    LogEqualTo = 0,
    /// `log:notEqualTo` — RDF term non-identity.
    LogNotEqualTo = 1,
    /// `math:sum`.
    MathSum = 2,
    /// `math:difference`.
    MathDifference = 3,
    /// `math:product`.
    MathProduct = 4,
    /// `math:quotient`.
    MathQuotient = 5,
    /// `string:concatenation`.
    StringConcatenation = 6,
    /// `list:member`.
    ListMember = 7,
}

impl N3Builtin {
    /// Every whitelist-able builtin, in declaration order.
    pub const ALL: [N3Builtin; 8] = [
        N3Builtin::LogEqualTo,
        N3Builtin::LogNotEqualTo,
        N3Builtin::MathSum,
        N3Builtin::MathDifference,
        N3Builtin::MathProduct,
        N3Builtin::MathQuotient,
        N3Builtin::StringConcatenation,
        N3Builtin::ListMember,
    ];

    /// The bit this builtin occupies in a whitelist mask (`1 << discriminant`).
    ///
    /// # Complexity
    /// O(1).
    pub const fn mask_bit(self) -> u8 {
        1 << (self as u8)
    }

    /// The builtin's canonical SWAP IRI, used as hash/refusal-message
    /// material.
    ///
    /// # Complexity
    /// O(1).
    pub const fn iri(self) -> &'static str {
        match self {
            N3Builtin::LogEqualTo => "http://www.w3.org/2000/10/swap/log#equalTo",
            N3Builtin::LogNotEqualTo => "http://www.w3.org/2000/10/swap/log#notEqualTo",
            N3Builtin::MathSum => "http://www.w3.org/2000/10/swap/math#sum",
            N3Builtin::MathDifference => "http://www.w3.org/2000/10/swap/math#difference",
            N3Builtin::MathProduct => "http://www.w3.org/2000/10/swap/math#product",
            N3Builtin::MathQuotient => "http://www.w3.org/2000/10/swap/math#quotient",
            N3Builtin::StringConcatenation => "http://www.w3.org/2000/10/swap/string#concatenation",
            N3Builtin::ListMember => "http://www.w3.org/2000/10/swap/list#member",
        }
    }
}

/// A recognized N3 builtin classified as **actuation-triggering** (real I/O,
/// network, or process dispatch — a side effect outside pure graph
/// reasoning), drawn from the `log:`/`http:` corners of the SWAP builtin
/// vocabularies (<https://www.w3.org/2000/10/swap/>) that [`N3Builtin`]
/// deliberately excludes. PRD §12 requires "zero direct actuation" from N3
/// (`PRD.md:604,608`); this type exists so [`N3Executor::run`] has something
/// concrete to check and refuse — a caller-declared reference to a variant
/// here is refused unconditionally by
/// [`Refusal::N3DirectActuationRefused`], the same way for every profile,
/// with no whitelist mask that could ever admit it (contrast
/// [`N3Builtin`], whose mask-gated membership *can* be widened by a
/// profile).
///
/// Deliberately **not** mask-based (unlike [`N3Builtin::mask_bit`]): there is
/// no notion of a profile "enabling" a direct-actuation builtin, so no bit
/// convention is needed, and this enum is free to grow past eight variants
/// without colliding with the `u8` whitelist-mask trick.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize)]
pub enum N3ActuationBuiltin {
    /// `log:webOperation` — issues a real HTTP request as a side effect.
    LogWebOperation,
    /// `log:semantics` — dereferences a URI and parses the fetched document,
    /// a real network fetch rather than pure graph reasoning.
    LogSemantics,
    /// `os:process` — spawns an operating-system process.
    OsProcess,
}

impl N3ActuationBuiltin {
    /// Every recognized direct-actuation builtin, in declaration order.
    pub const ALL: [N3ActuationBuiltin; 3] = [
        N3ActuationBuiltin::LogWebOperation,
        N3ActuationBuiltin::LogSemantics,
        N3ActuationBuiltin::OsProcess,
    ];

    /// The builtin's canonical SWAP IRI, used as hash/refusal-message
    /// material.
    ///
    /// # Complexity
    /// O(1).
    pub const fn iri(self) -> &'static str {
        match self {
            N3ActuationBuiltin::LogWebOperation => {
                "http://www.w3.org/2000/10/swap/log#webOperation"
            }
            N3ActuationBuiltin::LogSemantics => "http://www.w3.org/2000/10/swap/log#semantics",
            N3ActuationBuiltin::OsProcess => "http://www.w3.org/2000/10/swap/os#process",
        }
    }
}

/// The N3-specific execution parameters a profile declares once it enables
/// N3 at all (via [`ProfileGates::DEFAULT_ENABLED_MASK`]-overriding
/// enablement, checked by [`N3Executor::run`]). Distinct from
/// [`ProfileGates`], which gates *whether* N3 may run; this struct gates
/// *what* an admitted N3 execution may reference ([`N3Builtin`] whitelist)
/// and *how much* bounded work it may spend ([`N3CostBound`]).
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct N3ExecutionProfile {
    /// Bitmask of [`N3Builtin`] variants this execution may reference.
    pub builtin_whitelist_mask: u8,
    /// Declared cumulative cost ceiling, in [`N3Ticks`], for one execution.
    pub cost_bound_ticks: N3Ticks,
}

impl N3ExecutionProfile {
    /// Whether `builtin` is in this execution's whitelist.
    ///
    /// # Complexity
    /// O(1).
    pub const fn is_builtin_permitted(&self, builtin: N3Builtin) -> bool {
        self.builtin_whitelist_mask & builtin.mask_bit() != 0
    }
}

/// One N3 rule as [`N3Executor`]'s controlled execution surface sees it: not
/// a full N3 parse tree (an N3 interpreter is out of this ticket's scope —
/// this router models *enforcement*, not evaluation), but the facts PRD §12
/// gates on: which builtins the rule body invokes, its declared execution
/// cost, and (PROJ-780) whether the rule body invokes any recognized
/// direct-actuation builtin.
#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct N3Rule {
    /// Stable identifier for this rule (carried into refusal messages and
    /// the execution receipt).
    pub rule_id: String,
    /// Builtins the rule body invokes. Order-independent; a rule invoking
    /// zero builtins is legal (a bare graph-pattern implication).
    pub builtins: Vec<N3Builtin>,
    /// Declared execution cost, in [`N3Ticks`]. Declared, not measured —
    /// the same non-negotiable as `Ticks` in the `praxis-synthesis` budget
    /// convention: no wall clock, no measured CPU cycles in a
    /// receipt-adjacent path.
    pub declared_cost: N3Ticks,
    /// Recognized direct-actuation builtins ([`N3ActuationBuiltin`]) this
    /// rule body invokes, if any. Order-independent; empty for the ordinary
    /// pure-reasoning rule. Caller-declared classification data, the same
    /// convention as `builtins`/`declared_cost` (`N3Executor` does not parse
    /// N3 syntax — see the struct-level doc). Any non-empty value here is
    /// refused unconditionally by [`N3Executor::run`]
    /// ([`Refusal::N3DirectActuationRefused`]), independent of
    /// `builtin_whitelist_mask` or `cost_bound_ticks` — PRD §12: "An N3 rule
    /// that requests direct actuation SHALL be refused" (`PRD.md:608`).
    pub direct_actuation_builtins: Vec<N3ActuationBuiltin>,
}

/// The sealed result of [`N3Executor::run`] admitting and running a sequence
/// of [`N3Rule`]s.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct N3ExecutionReceipt {
    /// Rule IDs admitted, in execution order.
    pub rules_admitted: Vec<String>,
    /// Total ticks consumed across all admitted rules.
    pub ticks_used: N3Ticks,
    /// Digest binding (profile, rules admitted, ticks used) together.
    pub execution_hash: Digest,
}

/// Runs [`N3Rule`]s under a fixed [`ProfileGates`] and [`N3ExecutionProfile`],
/// enforcing PRD §12's capability gate, direct-actuation refusal, builtin
/// whitelist, and cost bound.
#[derive(Debug, Clone, Copy)]
pub struct N3Executor<'a> {
    gates: &'a ProfileGates,
    execution: &'a N3ExecutionProfile,
}

impl<'a> N3Executor<'a> {
    /// Builds an executor over a fixed gates/execution-profile pair. Neither
    /// is copied or validated against the other; the caller is responsible
    /// for pairing the `execution` parameters with the `gates` they are
    /// meant to govern (mirrors [`DialectRouter::new`], which likewise takes
    /// [`ProfileGates`] as given).
    pub fn new(gates: &'a ProfileGates, execution: &'a N3ExecutionProfile) -> Self {
        Self { gates, execution }
    }

    /// Admits and runs `rules` in order under the controlled execution
    /// surface.
    ///
    /// # Errors
    /// - [`Refusal::N3UnavailableByProfile`] — the profile does not enable
    ///   N3 at all; no rule is inspected.
    /// - [`Refusal::N3DirectActuationRefused`] — some rule declares a
    ///   recognized direct-actuation builtin
    ///   ([`N3Rule::direct_actuation_builtins`]); refused unconditionally,
    ///   before that rule's ordinary builtin whitelist is checked or its
    ///   cost is consumed (and before any later rule runs) — no
    ///   `execution.builtin_whitelist_mask` value can ever admit it.
    /// - [`Refusal::N3BuiltinRefused`] — some rule references a builtin
    ///   outside `execution`'s whitelist; refused before that rule's cost is
    ///   consumed (and before any later rule runs).
    /// - [`Refusal::N3CostBoundExceeded`] — cumulative declared cost across
    ///   `rules`, taken in order, exceeds `execution.cost_bound_ticks`.
    ///   Cost is tracked incrementally via [`N3CostBound::consume`] rule by
    ///   rule (not a single pre-declared-total check performed once), so a
    ///   single over-cost rule is rejected before any rule runs, and a
    ///   later rule that pushes an otherwise-within-budget running total
    ///   over the bound is caught mid-execution by the same accounting.
    ///
    /// # Complexity
    /// O(R * (A + B)) where R = `rules.len()`, A = direct-actuation builtins
    /// per rule (bounded by [`N3ActuationBuiltin::ALL`]'s fixed 3-variant
    /// size for any well-formed rule), and B = ordinary builtins per rule,
    /// bounded by [`N3Builtin::ALL`]'s fixed 8-variant size for any
    /// well-formed rule. Sealing the receipt on success hashes the admitted
    /// rule IDs and the profile identity, O(|profile_id| + sum of admitted
    /// rule ID lengths).
    pub fn run(&self, rules: &[N3Rule]) -> Result<N3ExecutionReceipt, Refusal> {
        if !self.gates.is_enabled(Dialect::N3) {
            return Err(Refusal::N3UnavailableByProfile(format!(
                "profile {}: N3 execution requested but the profile does not enable N3 \
                 (enabled mask {:#010b})",
                self.gates.profile_id, self.gates.enabled_dialects_mask
            )));
        }

        let mut bound = N3CostBound::new(self.execution.cost_bound_ticks);
        let mut admitted: Vec<String> = Vec::with_capacity(rules.len());

        // O(R * (A + B)): one pass over rules, each checking its declared
        // direct-actuation builtins, then its bounded ordinary builtin list,
        // before consuming its declared cost.
        for rule in rules {
            if let Some(actuation_builtin) = rule.direct_actuation_builtins.first() {
                return Err(Refusal::N3DirectActuationRefused(format!(
                    "profile {}: rule {} requests direct actuation via builtin {}; N3 rules \
                     may never drive actuation regardless of any execution whitelist",
                    self.gates.profile_id,
                    rule.rule_id,
                    actuation_builtin.iri()
                )));
            }
            for &builtin in &rule.builtins {
                if !self.execution.is_builtin_permitted(builtin) {
                    return Err(Refusal::N3BuiltinRefused(format!(
                        "profile {}: rule {} invokes builtin {} which is outside the N3 \
                         execution whitelist (mask {:#010b})",
                        self.gates.profile_id,
                        rule.rule_id,
                        builtin.iri(),
                        self.execution.builtin_whitelist_mask
                    )));
                }
            }
            let within_bound = bound.consume(rule.declared_cost);
            if !within_bound {
                return Err(Refusal::N3CostBoundExceeded(format!(
                    "profile {}: rule {} pushed cumulative cost to {} ticks, exceeding the \
                     declared bound of {} ticks ({} rule(s) already admitted)",
                    self.gates.profile_id,
                    rule.rule_id,
                    bound.used.0,
                    bound.limit.0,
                    admitted.len()
                )));
            }
            admitted.push(rule.rule_id.clone());
        }

        Ok(self.seal(admitted, bound.used))
    }

    /// Builds the sealed receipt for an admitted execution. Material is
    /// version- and field-tagged with each admitted rule ID passed as its
    /// own length-prefixed element (never joined into one string), so the
    /// hash is injective over the ordered rule-ID sequence; same (gates,
    /// rules admitted, ticks used) → byte-identical digest.
    ///
    /// # Complexity
    /// O(|profile_id| + sum of admitted rule ID lengths).
    fn seal(&self, rules_admitted: Vec<String>, ticks_used: N3Ticks) -> N3ExecutionReceipt {
        let profile_hash = self.gates.hash();
        let ticks_str = ticks_used.0.to_string();
        let mut material: Vec<&str> = Vec::with_capacity(5 + rules_admitted.len() * 2);
        material.push(N3_EXECUTION_HASH_TAG);
        material.push("profile_hash");
        material.push(&profile_hash.0);
        for rule_id in &rules_admitted {
            material.push("rule");
            material.push(rule_id.as_str());
        }
        material.push("ticks_used");
        material.push(&ticks_str);
        let execution_hash = Digest::new(blake3_combined(&material));
        N3ExecutionReceipt {
            rules_admitted,
            ticks_used,
            execution_hash,
        }
    }
}

#[cfg(test)]
#[path = "router_test.rs"]
mod tests;
