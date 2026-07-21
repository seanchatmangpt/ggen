//! Receipt schema epoch v2: an itemized admission ledger, a monotonic
//! standing ceiling, a closed artifact-equivalence map, and a
//! precedence-derived Andon level -- layered onto [`crate::receipt_record::ReceiptRecord`]
//! without breaking v1 read compatibility.
//!
//! # Design summary
//!
//! [`crate::receipt_record::ReceiptRecord`] gained exactly two new fields to
//! carry this epoch: `schema` (a string identity, defaulting to
//! [`SCHEMA_V1`] so every pre-existing receipt on disk still deserializes)
//! and `v2` (an `Option<ReceiptEpochV2>`, `None` on every v1 record). This is
//! the additive-fields design named in the task over a wholly parallel
//! struct: it costs each of the eight pre-existing `ReceiptRecord { .. }`
//! construction sites two extra lines (`schema: SCHEMA_V1.into(), v2: None`)
//! and changes nothing about chain-hash computation --
//! [`crate::law::ReceiptMeta`]/`build_admission_frame` never read either
//! field, so [`crate::receipt_record::ReceiptRecord::recompute_chain_hash`]
//! is bit-for-bit unchanged for old and new records alike.
//!
//! # Dual-read / single-write
//!
//! The writer ([`crate::sync`]'s `write_receipt`, `ggen-engine`) emits only
//! `schema = SCHEMA_V2` with a populated `v2` payload going forward. The
//! reader is [`read_receipt_epoch`]: given a v1 record (schema absent or
//! [`SCHEMA_V1`], `v2: None`) it returns [`ReceiptEpochV2::legacy_bounded`] --
//! never a value derived from the v1 record's own hardcoded
//! `andon: Andon::Green`. Given a v2 record it returns the real payload.
//! Given anything else (an unrecognized/future schema string, or a schema
//! string that contradicts whether `v2` is present) it refuses with a real
//! [`crate::error::CoreError`], never a silent default.
//!
//! [`ReceiptRecordV1Legacy`] models a *pre-migration* reader (the strict,
//! `deny_unknown_fields` wire shape `ReceiptRecord` had before this module
//! existed) so the "an old v1-only binary refuses to parse a v2 receipt" half
//! of the contract is testable directly, not asserted.

use serde::{Deserialize, Serialize};

use crate::error::CoreError;

/// Schema identity for the pre-epoch wire shape: no `v2` payload, andon is
/// whatever [`crate::law::Andon`] the emitting law object carried (routinely
/// a hardcoded `Green` at simple chain-only call sites, e.g. `ggen sync`'s
/// prior `write_receipt`).
pub const SCHEMA_V1: &str = "ggen-receipt/v1";

/// Schema identity for the epoch-v2 wire shape: `v2` is populated with a real
/// admission ledger, standing ceiling, equivalence map, and derived Andon.
pub const SCHEMA_V2: &str = "ggen-receipt/v2";

pub(crate) fn default_schema() -> String {
    SCHEMA_V1.to_string()
}

// ---------------------------------------------------------------------------
// AndonLevel -- the v2-only Red/Yellow/Green precedence lattice
// ---------------------------------------------------------------------------

/// The v2 Andon precedence lattice. Declaration order is significant: `derive(Ord)`
/// makes `Red < Yellow < Green`, so `.min()` over a set of levels implements
/// the required "Red beats Yellow beats Green" aggregation with no separate
/// comparison function to keep in sync.
///
/// Distinct from [`crate::law::Andon`] (`Green`/`Halted`/`Overridden`), which
/// stays exactly as it was -- `AndonLevel` is the new v2 payload's derived
/// field, never a replacement for the lifecycle halt/override mechanism.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Serialize, Deserialize)]
pub enum AndonLevel {
    /// At least one admission item was refused this generation.
    Red,
    /// No refusals, but at least one admission item was quarantined.
    Yellow,
    /// Every admission item was cleanly admitted.
    Green,
}

// ---------------------------------------------------------------------------
// CeilingLevel -- standing_ceiling's lattice, with a legacy sentinel
// ---------------------------------------------------------------------------

/// The standing-ceiling lattice. `LegacyObserved` sits strictly between `Red`
/// and `Yellow`: a chain that has ever passed through a v1 (unitemized)
/// generation is capped there and, under the identity `recoverable` policy
/// documented on [`recoverable`], can never climb back to `Green` by mere
/// meet-based inference -- only an explicit migration/override event (outside
/// this module's scope) could reset it.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Serialize, Deserialize)]
pub enum CeilingLevel {
    /// At least one component/admission signal was Red.
    Red,
    /// Bounded by an unitemized v1-epoch ancestor; never promotable to Green
    /// by inference (see [`recoverable`]).
    LegacyObserved,
    /// No Red, but at least one component/admission signal was Yellow.
    Yellow,
    /// Every component/admission signal was Green. The lattice's top
    /// element: `meet(Green, x) == x` for any `x`, which is why a genesis
    /// (no-prior-receipt) chain starts `recoverable`/`prev_ceiling` at
    /// `Green` -- there is no prior evidence to constrain it yet.
    Green,
}

impl From<AndonLevel> for CeilingLevel {
    fn from(level: AndonLevel) -> Self {
        match level {
            AndonLevel::Red => CeilingLevel::Red,
            AndonLevel::Yellow => CeilingLevel::Yellow,
            AndonLevel::Green => CeilingLevel::Green,
        }
    }
}

/// Independent component results the standing ceiling is a meet over.
///
/// Grounded in this repo's own gates rather than invented: `just pre-commit`
/// (see `.claude/rules/andon/signals.md`) chains nine checks, which this
/// type collapses to four named components for the receipt -- `lint`
/// (`cargo clippy`), `test` (`test-lib`), `fmt` (`fmt-check`), and `gate`
/// (an aggregate of the remaining structural gates: `check`,
/// `coherence-check`, and the `guard-*` gates). A caller with finer-grained
/// evidence can still set each field independently; `gate` exists so a
/// caller with only "everything else passed/failed" evidence isn't forced to
/// fabricate a specific gate name.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub struct ComponentLevels {
    /// `cargo clippy --all-targets -- -D warnings` result.
    pub lint: AndonLevel,
    /// `test-lib` (or the full suite, if run) result.
    pub test: AndonLevel,
    /// `fmt-check` result.
    pub fmt: AndonLevel,
    /// Aggregate of the remaining `just pre-commit` gates not named above.
    pub gate: AndonLevel,
}

impl ComponentLevels {
    /// All four components at the same level -- the honest choice at a call
    /// site (like `ggen sync`'s receipt writer) that has exactly one signal
    /// available (its own admission-derived level) and no independent
    /// lint/test/fmt/gate evidence to report; claiming per-component Green
    /// there would overclaim checks that were never run.
    #[must_use]
    pub fn uniform(level: AndonLevel) -> Self {
        Self {
            lint: level,
            test: level,
            fmt: level,
            gate: level,
        }
    }
}

/// `supported(evidence)`: the ceiling this generation's component evidence
/// alone would support, with no reference to history.
#[must_use]
pub fn supported(components: &ComponentLevels) -> CeilingLevel {
    [
        components.lint,
        components.test,
        components.fmt,
        components.gate,
    ]
    .into_iter()
    .map(CeilingLevel::from)
    .min()
    .unwrap_or(CeilingLevel::Green)
}

/// `recoverable(ceiling_n)`: how much of the previous generation's ceiling
/// carries forward into this generation's meet, before combining with
/// [`supported`].
///
/// The current policy is the identity function: nothing decays and nothing
/// is inferred back up. This is deliberate, not an oversight -- it is what
/// makes `LegacyObserved` a genuine one-way cap (see
/// [`CeilingLevel::LegacyObserved`]) rather than a value that quietly heals
/// itself after a few clean generations. A future policy that allows
/// supervised promotion off `LegacyObserved` would replace this function,
/// not [`compute_ceiling`]'s shape.
#[must_use]
pub fn recoverable(prev: CeilingLevel) -> CeilingLevel {
    prev
}

/// The ceiling monotonicity rule: `ceiling_{n+1} = meet(recoverable(ceiling_n), supported(evidence_{n+1}))`.
///
/// `meet` is `min` under [`CeilingLevel`]'s derived `Ord` -- the same
/// worst-of relation [`AndonLevel`] uses, extended with the `LegacyObserved`
/// sentinel ranked between `Red` and `Yellow`.
#[must_use]
pub fn compute_ceiling(prev: CeilingLevel, components: &ComponentLevels) -> CeilingLevel {
    recoverable(prev).min(supported(components))
}

// ---------------------------------------------------------------------------
// Equivalence -- a closed, always-complete map over 8 artifact classes
// ---------------------------------------------------------------------------

/// Per-class equivalence status. `Unknown` is a first-class, explicit value
/// (never represented by field absence -- see [`EquivalenceMap`]).
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub enum EquivalenceStatus {
    /// This generation did not evaluate this artifact class.
    Unknown,
    /// This artifact class was evaluated and found equivalent (unchanged in
    /// the sense relevant to that class).
    Equivalent,
    /// This artifact class was evaluated and found divergent, with the
    /// reason recorded rather than dropped.
    Divergent(String),
}

/// A closed set over 8 artifact classes, one field per class so the type
/// itself -- not a runtime check -- guarantees completeness: there is no
/// value of this type missing a class, and no `#[serde(default)]` on any
/// field, so a hand-edited (or corrupted) JSON object missing one of the
/// eight keys fails to deserialize with a real `serde_json` "missing field"
/// error rather than silently defaulting.
///
/// The 8 classes are the ones the task names and this codebase's own
/// vocabulary already distinguishes: source (`.rs`/`.ttl`/template inputs),
/// compiled-binary (`target/`), docs (`docs/`, `book/`), tests
/// (`tests/`/`#[test]`), receipts (`.ggen-v2/receipt*.json`), evidence (OCEL
/// events, `.ggen/ocel/agent-edit-events.ocel.jsonl`), gates (`just
/// pre-commit`'s checks), and config (`ggen.toml`, `.specify/*.ttl`).
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
#[serde(deny_unknown_fields)]
pub struct EquivalenceMap {
    /// Source files (ontologies, templates, hand-written Rust).
    pub source: EquivalenceStatus,
    /// Compiled build artifacts.
    pub compiled_binary: EquivalenceStatus,
    /// Documentation (`docs/`, `book/`).
    pub docs: EquivalenceStatus,
    /// Test files and their outcomes.
    pub tests: EquivalenceStatus,
    /// Prior receipts in the chain.
    pub receipts: EquivalenceStatus,
    /// Process/OCEL evidence.
    pub evidence: EquivalenceStatus,
    /// `just pre-commit` gate results.
    pub gates: EquivalenceStatus,
    /// Configuration (`ggen.toml`, `.specify/*.ttl`).
    pub config: EquivalenceStatus,
}

impl EquivalenceMap {
    /// Every class explicitly `Unknown` -- the legacy-bounded reader's
    /// value, and a convenient starting point for callers building a real
    /// one incrementally.
    #[must_use]
    pub fn all_unknown() -> Self {
        Self {
            source: EquivalenceStatus::Unknown,
            compiled_binary: EquivalenceStatus::Unknown,
            docs: EquivalenceStatus::Unknown,
            tests: EquivalenceStatus::Unknown,
            receipts: EquivalenceStatus::Unknown,
            evidence: EquivalenceStatus::Unknown,
            gates: EquivalenceStatus::Unknown,
            config: EquivalenceStatus::Unknown,
        }
    }
}

// ---------------------------------------------------------------------------
// Admission ledger
// ---------------------------------------------------------------------------

/// What was actually observed for one admitted evidence item, independent of
/// what the system decided to do about it (see [`AdmissionDecision`]).
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub enum ObservedOutcome {
    /// The evidence item passed whatever check produced it.
    Pass,
    /// The evidence item failed whatever check produced it.
    Fail,
    /// No pass/fail outcome was observed (e.g. an informational item).
    Unknown,
}

/// What the system decided to do with one admitted evidence item. Precedence
/// for the derived [`AndonLevel`] follows directly from [`AdmissionDecision::level`].
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub enum AdmissionDecision {
    /// Admitted cleanly.
    Admitted,
    /// Admitted provisionally; downstream promotion is blocked until
    /// resolved (see [`ReceiptEpochV2::promotion_eligible`]).
    Quarantined,
    /// Refused outright.
    Refused,
}

impl AdmissionDecision {
    /// This decision's contribution to the derived [`AndonLevel`].
    #[must_use]
    pub fn level(&self) -> AndonLevel {
        match self {
            AdmissionDecision::Admitted => AndonLevel::Green,
            AdmissionDecision::Quarantined => AndonLevel::Yellow,
            AdmissionDecision::Refused => AndonLevel::Red,
        }
    }
}

/// One entry in the v2 admission ledger: one admitted evidence item, its
/// observed outcome, the decision made about it, why, and which obligations
/// it discharged or created.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct AdmissionItem {
    /// Identifier for the evidence item (e.g. an output path, a gate name).
    pub evidence_id: String,
    /// What was observed.
    pub observed_outcome: ObservedOutcome,
    /// What was decided.
    pub decision: AdmissionDecision,
    /// Why. Never empty in a well-formed item, but not type-enforced here --
    /// callers own that judgment call.
    pub reason: String,
    /// Obligation identifiers this item discharged.
    #[serde(default)]
    pub obligations_discharged: Vec<String>,
    /// Obligation identifiers this item created.
    #[serde(default)]
    pub obligations_created: Vec<String>,
}

/// The admission ledger: either a genuine v2 itemization (possibly empty --
/// zero evidence items were admitted this generation, which is a real,
/// known fact) or [`AdmissionLedger::LegacyUnrecorded`], a sentinel meaning
/// "this generation predates itemized admission tracking entirely." The two
/// are deliberately not conflatable: an empty `Recorded(vec![])` and
/// `LegacyUnrecorded` carry different information and must never compare
/// equal in meaning even though a naive `Vec::is_empty()` check could
/// otherwise blur them.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub enum AdmissionLedger {
    /// Predates itemized admission tracking (a v1-epoch ancestor).
    LegacyUnrecorded,
    /// A real, possibly-empty itemization for this generation.
    Recorded(Vec<AdmissionItem>),
}

/// Derive the aggregate [`AndonLevel`] from an admission ledger, honoring
/// strict `Red > Yellow > Green` precedence: any single refused item forces
/// the aggregate to `Red`, any single quarantined item (with no refusals)
/// forces `Yellow`, otherwise `Green`. `LegacyUnrecorded` never derives
/// `Green` -- this is the core invariant the whole module exists to
/// guarantee: a v1 receipt's hardcoded `Andon::Green` must never leak into a
/// derived v2 `Green`. `Yellow`, not `Red`, is used for the legacy case: a
/// legacy receipt did not necessarily fail anything (that would overclaim a
/// negative finding that was never made) -- it is bounded/unverified, which
/// is exactly what `Yellow` (quarantined-equivalent) means here.
#[must_use]
pub fn derive_andon(ledger: &AdmissionLedger) -> AndonLevel {
    match ledger {
        AdmissionLedger::LegacyUnrecorded => AndonLevel::Yellow,
        AdmissionLedger::Recorded(items) => items
            .iter()
            .map(AdmissionItem::level_ref)
            .min()
            .unwrap_or(AndonLevel::Green),
    }
}

impl AdmissionItem {
    fn level_ref(item: &AdmissionItem) -> AndonLevel {
        item.decision.level()
    }
}

// ---------------------------------------------------------------------------
// Obligation count -- computed AFTER admission processing
// ---------------------------------------------------------------------------

/// Obligation accounting for one generation, computed strictly after
/// admission processing (never guessed ahead of it).
///
/// `Unknown` and `Tracked { required: 0, discharged: 0 }` are distinct
/// values: the former means "this generation's obligation state was never
/// itemized" (the legacy case); the latter means "zero evidence items were
/// admitted, and zero obligations were required as a result" -- a real,
/// explicit fact, not a default standing in for ignorance.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum ObligationCount {
    /// Obligation state was not itemized this generation (legacy-bounded).
    Unknown,
    /// `required` obligation-identifier occurrences created this generation,
    /// `discharged` occurrences resolved. Both explicit so "0 remaining" is
    /// distinguishable from "nothing was ever tracked."
    Tracked {
        /// Total obligation-identifier occurrences created.
        required: u32,
        /// Total obligation-identifier occurrences discharged.
        discharged: u32,
    },
}

impl ObligationCount {
    /// `required - discharged`, saturating at 0. `None` for `Unknown`.
    #[must_use]
    pub fn remaining(&self) -> Option<u32> {
        match self {
            ObligationCount::Unknown => None,
            ObligationCount::Tracked {
                required,
                discharged,
            } => Some(required.saturating_sub(*discharged)),
        }
    }
}

/// Compute [`ObligationCount`] from the admission ledger, after processing
/// every item: `required` is the total count of `obligations_created`
/// entries across all items (not deduplicated -- each occurrence is a real
/// discharge/creation event), `discharged` the total count of
/// `obligations_discharged` entries.
#[must_use]
pub fn compute_obligation_count(ledger: &AdmissionLedger) -> ObligationCount {
    match ledger {
        AdmissionLedger::LegacyUnrecorded => ObligationCount::Unknown,
        AdmissionLedger::Recorded(items) => {
            let required: u32 = items
                .iter()
                .map(|i| i.obligations_created.len() as u32)
                .sum();
            let discharged: u32 = items
                .iter()
                .map(|i| i.obligations_discharged.len() as u32)
                .sum();
            ObligationCount::Tracked {
                required,
                discharged,
            }
        }
    }
}

// ---------------------------------------------------------------------------
// ReceiptEpochV2 -- the v2 payload
// ---------------------------------------------------------------------------

/// The v2-only payload carried by [`crate::receipt_record::ReceiptRecord::v2`].
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
#[serde(deny_unknown_fields)]
pub struct ReceiptEpochV2 {
    /// The itemized admission ledger.
    pub admission: AdmissionLedger,
    /// The standing ceiling after this generation (see [`compute_ceiling`]).
    pub standing_ceiling: CeilingLevel,
    /// The closed 8-class artifact-equivalence map.
    pub equivalence: EquivalenceMap,
    /// Obligation accounting, computed after admission processing.
    pub obligation_count: ObligationCount,
    /// The derived Andon level (see [`derive_andon`]).
    pub andon: AndonLevel,
    /// Whether this generation is eligible for promotion (e.g. off a
    /// quarantined/legacy state). Always `false` for a legacy-bounded
    /// reading; `false` for any generation with a quarantined admission
    /// item; `true` otherwise.
    pub promotion_eligible: bool,
}

impl ReceiptEpochV2 {
    /// The legacy-bounded reading of a v1 receipt: never derived from the
    /// v1 record's own fields (in particular never from its hardcoded
    /// `Andon::Green`), always the same fixed, honest sentinel value.
    #[must_use]
    pub fn legacy_bounded() -> Self {
        let admission = AdmissionLedger::LegacyUnrecorded;
        let andon = derive_andon(&admission);
        let obligation_count = compute_obligation_count(&admission);
        Self {
            admission,
            standing_ceiling: CeilingLevel::LegacyObserved,
            equivalence: EquivalenceMap::all_unknown(),
            obligation_count,
            andon,
            promotion_eligible: false,
        }
    }
}

/// Builder for a genuine v2 [`ReceiptEpochV2`]: the only way to construct
/// one outside of [`ReceiptEpochV2::legacy_bounded`], so `andon`,
/// `obligation_count`, and (absent an explicit override) `standing_ceiling`
/// are always *computed* from the admission ledger and component evidence,
/// never hand-set to a value the ledger doesn't support.
pub struct ReceiptEpochV2Builder {
    prev_ceiling: CeilingLevel,
    components: ComponentLevels,
    admission: Vec<AdmissionItem>,
    equivalence: EquivalenceMap,
    explicit_ceiling: Option<CeilingLevel>,
}

impl ReceiptEpochV2Builder {
    /// Start a builder chained onto `prev_ceiling` (the previous
    /// generation's standing ceiling; use `CeilingLevel::Green` at genesis --
    /// the lattice's top/identity element) with this generation's component
    /// evidence.
    #[must_use]
    pub fn new(prev_ceiling: CeilingLevel, components: ComponentLevels) -> Self {
        Self {
            prev_ceiling,
            components,
            admission: Vec::new(),
            equivalence: EquivalenceMap::all_unknown(),
            explicit_ceiling: None,
        }
    }

    /// Append one admission item.
    #[must_use]
    pub fn admission_item(mut self, item: AdmissionItem) -> Self {
        self.admission.push(item);
        self
    }

    /// Set the full equivalence map (defaults to all-`Unknown`).
    #[must_use]
    pub fn equivalence(mut self, map: EquivalenceMap) -> Self {
        self.equivalence = map;
        self
    }

    /// Force a specific ceiling instead of the computed
    /// `meet(recoverable(prev), supported(components))`. Refused at
    /// [`Self::build`] time if it exceeds that meet -- see
    /// [`crate::error::CoreError::CeilingExceedsMeet`].
    #[must_use]
    pub fn with_explicit_ceiling(mut self, level: CeilingLevel) -> Self {
        self.explicit_ceiling = Some(level);
        self
    }

    /// Compute the final [`ReceiptEpochV2`], refusing rather than silently
    /// clamping if an explicit ceiling override exceeds what the evidence
    /// supports.
    pub fn build(self) -> Result<ReceiptEpochV2, CoreError> {
        let allowed = compute_ceiling(self.prev_ceiling, &self.components);
        let standing_ceiling = match self.explicit_ceiling {
            Some(requested) if requested > allowed => {
                return Err(CoreError::CeilingExceedsMeet { requested, allowed })
            }
            Some(requested) => requested,
            None => allowed,
        };
        let ledger = AdmissionLedger::Recorded(self.admission);
        let andon = derive_andon(&ledger);
        let obligation_count = compute_obligation_count(&ledger);
        let promotion_eligible = andon == AndonLevel::Green;
        Ok(ReceiptEpochV2 {
            admission: ledger,
            standing_ceiling,
            equivalence: self.equivalence,
            obligation_count,
            andon,
            promotion_eligible,
        })
    }
}

// ---------------------------------------------------------------------------
// Dual-read entrypoint
// ---------------------------------------------------------------------------

/// Read the v2 epoch view of a [`crate::receipt_record::ReceiptRecord`],
/// dispatching on its declared `schema`:
///
/// - [`SCHEMA_V1`] with no `v2` payload -> [`ReceiptEpochV2::legacy_bounded`].
/// - [`SCHEMA_V2`] with a `v2` payload -> that payload, verbatim.
/// - Anything else (an unrecognized schema string, or a schema/payload
///   contradiction) -> a real `Err`, never a silent default.
pub fn read_receipt_epoch(
    record: &crate::receipt_record::ReceiptRecord,
) -> Result<ReceiptEpochV2, CoreError> {
    match record.schema.as_str() {
        SCHEMA_V1 => {
            if record.v2.is_some() {
                return Err(CoreError::ReceiptSchemaPayloadMismatch(format!(
                    "schema `{SCHEMA_V1}` but a v2 payload is present"
                )));
            }
            Ok(ReceiptEpochV2::legacy_bounded())
        }
        SCHEMA_V2 => record.v2.clone().ok_or_else(|| {
            CoreError::ReceiptSchemaPayloadMismatch(format!(
                "schema `{SCHEMA_V2}` but no v2 payload is present"
            ))
        }),
        other => Err(CoreError::UnrecognizedReceiptSchema(other.to_string())),
    }
}

// ---------------------------------------------------------------------------
// ReceiptRecordV1Legacy -- models an old, pre-migration v1-only reader
// ---------------------------------------------------------------------------

/// The strict, `deny_unknown_fields` wire shape [`crate::receipt_record::ReceiptRecord`]
/// had before this module existed. Used only to model "an old v1-only
/// binary" in tests: deserializing a genuine v2 receipt's JSON into this
/// type must fail (the `schema`/`v2` keys are unknown to it), which is the
/// concrete, checkable half of "old code refuses new receipts" this module
/// promises rather than asserts.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
#[serde(deny_unknown_fields)]
pub struct ReceiptRecordV1Legacy {
    /// Schema version; mirrors `RECEIPT_RECORD_VERSION`.
    pub version: u32,
    /// Monotonically increasing step identity within a run.
    pub instruction_id: u64,
    /// Index into the activity table for this step's activity.
    pub activity_idx: u16,
    /// Resolved human-readable label for `activity_idx`, if available.
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub activity: Option<String>,
    /// Classifier byte for the POWL node kind.
    pub node_kind: u8,
    /// Wall-clock timestamp in nanoseconds.
    pub ts_ns: u64,
    /// Optional wall-clock duration in milliseconds.
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub duration_ms: Option<u64>,
    /// BLAKE3 hash of the canonical JSON payload bytes.
    pub payload_hash_hex: String,
    /// The chain hash this record was chained onto.
    pub prev_chain_hash_hex: String,
    /// The resulting chain hash after this record.
    pub chain_hash_hex: String,
    /// The Andon outcome at receipt time.
    pub andon: crate::law::Andon,
    /// Number of obligations attached to the law object at receipt time.
    pub obligation_count: u32,
    /// OCEL object identifiers this receipt governs.
    #[serde(default)]
    pub object_ids: Vec<String>,
    /// Hex-encoded ed25519 signature over `chain_hash_hex`.
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub signature_hex: Option<String>,
}

// ---------------------------------------------------------------------------
// Migration receipt M_1_to_2
// ---------------------------------------------------------------------------

/// The fixed migration-law identity for the v1 -> v2 boundary.
pub const MIGRATION_LAW_1_TO_2: &str = "M_1_to_2";

/// A single record binding the final v1 chain hash to the first v2 chain
/// hash: what carries forward, what becomes `Unknown`, and the resulting
/// (necessarily capped) ceiling.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
#[serde(deny_unknown_fields)]
pub struct MigrationReceipt {
    /// Fixed migration-law identity, always [`MIGRATION_LAW_1_TO_2`].
    pub migration_law: String,
    /// The schema identity being migrated from, always [`SCHEMA_V1`].
    pub from_schema: String,
    /// The schema identity being migrated to, always [`SCHEMA_V2`].
    pub to_schema: String,
    /// The final v1 receipt's chain hash (hex).
    pub final_v1_chain_hash_hex: String,
    /// The first v2 receipt's chain hash (hex), chained onto the field above.
    pub first_v2_chain_hash_hex: String,
    /// What information carries forward across the boundary.
    pub carries_forward: Vec<String>,
    /// What information becomes `Unknown` across the boundary (never
    /// silently dropped -- named explicitly).
    pub becomes_unknown: Vec<String>,
    /// The ceiling after migration -- always [`CeilingLevel::LegacyObserved`]:
    /// necessarily capped, because the v1 history it is built on was never
    /// itemized.
    pub resulting_ceiling: CeilingLevel,
}

impl MigrationReceipt {
    /// Construct the migration receipt binding `final_v1_chain_hash_hex` to
    /// `first_v2_chain_hash_hex`. `resulting_ceiling` is always
    /// [`CeilingLevel::LegacyObserved`] -- there is no constructor argument
    /// for it, so a migration receipt can never claim a higher ceiling than
    /// the v1 history actually supports.
    #[must_use]
    pub fn new(
        final_v1_chain_hash_hex: impl Into<String>, first_v2_chain_hash_hex: impl Into<String>,
    ) -> Self {
        Self {
            migration_law: MIGRATION_LAW_1_TO_2.to_string(),
            from_schema: SCHEMA_V1.to_string(),
            to_schema: SCHEMA_V2.to_string(),
            final_v1_chain_hash_hex: final_v1_chain_hash_hex.into(),
            first_v2_chain_hash_hex: first_v2_chain_hash_hex.into(),
            carries_forward: vec![
                "chain_hash_lineage".to_string(),
                "object_ids".to_string(),
                "instruction_id_sequence".to_string(),
            ],
            becomes_unknown: vec![
                "admission".to_string(),
                "equivalence".to_string(),
                "obligation_count".to_string(),
            ],
            resulting_ceiling: CeilingLevel::LegacyObserved,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn andon_level_precedence_is_red_yellow_green() {
        assert!(AndonLevel::Red < AndonLevel::Yellow);
        assert!(AndonLevel::Yellow < AndonLevel::Green);
    }

    #[test]
    fn ceiling_level_places_legacy_observed_between_red_and_yellow() {
        assert!(CeilingLevel::Red < CeilingLevel::LegacyObserved);
        assert!(CeilingLevel::LegacyObserved < CeilingLevel::Yellow);
        assert!(CeilingLevel::Yellow < CeilingLevel::Green);
    }

    #[test]
    fn legacy_observed_never_promotes_to_green_via_meet() {
        let components = ComponentLevels::uniform(AndonLevel::Green);
        let ceiling = compute_ceiling(CeilingLevel::LegacyObserved, &components);
        assert_eq!(ceiling, CeilingLevel::LegacyObserved);
    }

    #[test]
    fn genesis_prev_ceiling_green_is_the_meet_identity() {
        let components = ComponentLevels::uniform(AndonLevel::Yellow);
        let ceiling = compute_ceiling(CeilingLevel::Green, &components);
        assert_eq!(ceiling, CeilingLevel::Yellow);
    }

    #[test]
    fn equivalence_map_missing_a_class_fails_to_deserialize() {
        let complete = serde_json::to_value(EquivalenceMap::all_unknown()).unwrap();
        let mut incomplete = complete.as_object().unwrap().clone();
        incomplete.remove("config");
        let raw = serde_json::Value::Object(incomplete);
        let result: Result<EquivalenceMap, _> = serde_json::from_value(raw);
        assert!(result.is_err());
    }

    #[test]
    fn legacy_bounded_never_derives_green() {
        let epoch = ReceiptEpochV2::legacy_bounded();
        assert_ne!(epoch.andon, AndonLevel::Green);
        assert!(!epoch.promotion_eligible);
    }

    #[test]
    fn builder_refuses_ceiling_above_meet() {
        let components = ComponentLevels::uniform(AndonLevel::Yellow);
        let result = ReceiptEpochV2Builder::new(CeilingLevel::Green, components)
            .with_explicit_ceiling(CeilingLevel::Green)
            .build();
        assert!(matches!(result, Err(CoreError::CeilingExceedsMeet { .. })));
    }
}
