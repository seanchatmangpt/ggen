//! MAPE-K-style autonomic observation, diagnosis, and bounded intent planning.
//!
//! No function in this module performs actuation. The terminal output is an
//! [`ArchitectureIntent`] that must be admitted by BRCE or another explicit
//! broker before any external consequence occurs.

use std::collections::{BTreeMap, BTreeSet};

use serde::{Deserialize, Serialize};

use crate::{
    capacity::{CapacityEnvelope, CapacityLevel, CapacitySample},
    error::{ArchitectureError, Result},
    model::{LifecycleState, Severity, Standing},
    receipt::deterministic_hash,
    state::ArchitectureState,
};

/// Boundary stimulus admitted for one autonomic cycle.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
#[serde(tag = "kind", rename_all = "snake_case")]
pub enum Stimulus {
    /// New measured capacity evidence.
    Capacity {
        /// Observed sample.
        sample: CapacitySample,
    },
    /// An architecture asset changed.
    AssetChanged {
        /// Changed asset identifier.
        asset_id: String,
    },
    /// Generated or deployed state no longer matches its expected digest.
    DriftDetected {
        /// Drifted asset.
        asset_id: String,
        /// Expected digest.
        expected_hash: String,
        /// Observed digest.
        observed_hash: String,
    },
    /// A required architecture dependency is unavailable.
    DependencyUnavailable {
        /// Dependent asset.
        asset_id: String,
        /// Unavailable dependency.
        dependency_id: String,
    },
    /// A lifecycle deadline or externally approved target state arrived.
    LifecycleDeadline {
        /// Subject asset.
        asset_id: String,
        /// Requested lifecycle state.
        target: LifecycleState,
    },
    /// Evidence standing changed.
    StandingChanged {
        /// Subject asset.
        asset_id: String,
        /// Newly observed standing.
        standing: Standing,
    },
}

/// Diagnosis produced by analysis of an admitted stimulus.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct Diagnosis {
    /// Stable diagnosis code.
    pub code: String,
    /// Severity.
    pub severity: Severity,
    /// Primary subject.
    pub subject: String,
    /// Evidence-grounded rationale.
    pub rationale: String,
    /// Identified affected architecture assets.
    #[serde(default)]
    pub affected_assets: Vec<String>,
}

/// Kinds of bounded architecture intent.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum IntentKind {
    /// Surface an evidence-backed operator warning.
    Warn,
    /// Select or manufacture a smaller operating profile.
    Reprofile,
    /// Re-run architecture and implementation verification.
    Revalidate,
    /// Prevent promotion until evidence is repaired.
    BlockPromotion,
    /// Rebuild a deterministic generated projection.
    RebuildProjection,
    /// Construct a migration and rollback work package.
    CreateMigrationPlan,
    /// Recalculate a dependency-closed transition plan.
    ReplanTransition,
    /// Submit an otherwise-complete request to an admitted broker.
    SubmitToBroker,
}

/// Declarative, capability-bounded request produced by autonomic planning.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct ArchitectureIntent {
    /// Deterministic identity derived from the intent body.
    pub intent_id: String,
    /// Requested kind of consequence.
    pub kind: IntentKind,
    /// Primary subject.
    pub subject: String,
    /// Other affected architecture assets.
    #[serde(default)]
    pub affected_assets: Vec<String>,
    /// Preconditions the broker or downstream workflow must prove.
    #[serde(default)]
    pub preconditions: BTreeSet<String>,
    /// Capabilities required from an admitted actuator.
    #[serde(default)]
    pub required_capabilities: BTreeSet<String>,
    /// Evidence expected after lawful execution.
    #[serde(default)]
    pub expected_evidence: BTreeSet<String>,
    /// Stable parameters for downstream interpretation.
    #[serde(default)]
    pub payload: BTreeMap<String, String>,
}

#[derive(Serialize)]
struct IntentBody<'a> {
    kind: IntentKind,
    subject: &'a str,
    affected_assets: &'a [String],
    preconditions: &'a BTreeSet<String>,
    required_capabilities: &'a BTreeSet<String>,
    expected_evidence: &'a BTreeSet<String>,
    payload: &'a BTreeMap<String, String>,
}

impl ArchitectureIntent {
    fn build(
        kind: IntentKind, subject: String, mut affected_assets: Vec<String>,
        preconditions: BTreeSet<String>, required_capabilities: BTreeSet<String>,
        expected_evidence: BTreeSet<String>, payload: BTreeMap<String, String>,
    ) -> Result<Self> {
        affected_assets.sort();
        affected_assets.dedup();
        let intent_id = deterministic_hash(
            "architecture_intent",
            &IntentBody {
                kind,
                subject: &subject,
                affected_assets: &affected_assets,
                preconditions: &preconditions,
                required_capabilities: &required_capabilities,
                expected_evidence: &expected_evidence,
                payload: &payload,
            },
        )?;
        Ok(Self {
            intent_id,
            kind,
            subject,
            affected_assets,
            preconditions,
            required_capabilities,
            expected_evidence,
            payload,
        })
    }
}

/// Receipted result of one monitor-analyze-plan cycle.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct AutonomicCycle {
    /// Deterministic cycle identity.
    pub cycle_id: String,
    /// Caller-supplied observation timestamp or sequence label.
    pub observed_at: String,
    /// Admitted stimuli.
    pub stimuli: Vec<Stimulus>,
    /// Evidence-grounded diagnoses.
    pub diagnoses: Vec<Diagnosis>,
    /// Bounded intents, never direct effects.
    pub intents: Vec<ArchitectureIntent>,
    /// Constitutional proof that this cycle performed no actuation.
    pub actuation_performed: bool,
    /// BLAKE3 receipt over the complete cycle body.
    pub receipt_hash: String,
}

#[derive(Serialize)]
struct CycleReceiptBody<'a> {
    observed_at: &'a str,
    stimuli: &'a [Stimulus],
    diagnoses: &'a [Diagnosis],
    intents: &'a [ArchitectureIntent],
    actuation_performed: bool,
}

/// Read-only architecture controller implementing Monitor, Analyze, and Plan.
pub struct AutonomicController<'a> {
    state: &'a ArchitectureState,
}

impl<'a> AutonomicController<'a> {
    /// Bind the controller to one admitted architecture state.
    #[must_use]
    pub const fn new(state: &'a ArchitectureState) -> Self {
        Self { state }
    }

    /// Run one bounded autonomic cycle and emit intents only.
    pub fn run_cycle(
        &self, observed_at: impl Into<String>, stimuli: Vec<Stimulus>,
    ) -> Result<AutonomicCycle> {
        if self.state.autonomic_policy.direct_actuation_allowed {
            return Err(ArchitectureError::DirectActuationForbidden);
        }

        let observed_at = observed_at.into();
        let mut diagnoses = Vec::new();
        let mut intents = Vec::new();

        if self.state.autonomic_policy.enabled {
            for stimulus in &stimuli {
                self.analyze_stimulus(stimulus, &mut diagnoses, &mut intents)?;
            }
        } else {
  ²È="24€€€€€€€…ÍÍ•Ñ}¥°(€€€€€€€€€€€€€€€‘•Á•¹‘•¹å}¥°(€€€€€€€€€€€ô€ôøì(€€€€€€€€€€€€€€€±•Ğ…ÍÍ•Ğ€ôÍ•±˜¹ÍÑ…Ñ”¹É•¥ÍÑÉä¹…ÍÍ•Ğ¡…ÍÍ•Ñ}¥¤üì(€€€€€€€€€€€€€€€¥˜€……ÍÍ•Ğ¹‘•Á•¹‘•¹¥•Ì¹½¹Ñ…¥¹Ì¡‘•Á•¹‘•¹å}¥¤ì(€€€€€€€€€€€€€€€€€€€É•ÑÕÉ¸ÉÈ¡É¡¥Ñ•ÑÕÉ•ÉÉ½Èèé…¹±¥¹•Á•¹‘•¹äì(€€€€€€€€€€€€€€€€€€€€€€€…ÍÍ•Ñ}¥è…ÍÍ•Ñ}¥¹±½¹” ¤°(€€€€€€€€€€€€€€€€€€€€€€€‘•Á•¹‘•¹å}¥è‘•Á•¹‘•¹å}¥¹±½¹” ¤°(€€€€€€€€€€€€€€€€€€€ô¤ì(€€€€€€€€€€€€€€€ô(€€€€€€€€€€€€€€€‘¥…¹½Í•Ì¹ÁÕÍ ¡¥…¹½Í¥Ìì(€€€€€€€€€€€€€€€€€€€½‘”è€‰µUQ<´ĞÀÄˆ¹Ñ½}ÍÑÉ¥¹œ ¤°(€€€€€€€€€€€€€€€€€€€Í•Ù•É¥ÑäèM•Ù•É¥ÑäèéÉ¥Ñ¥…°°(€€€€€€€€€€€€€€€€€€€ÍÕ‰©•Ğè…ÍÍ•Ñ}¥¹±½¹” ¤°(€€€€€€€€€€€€€€€€€€€É…Ñ¥½¹…±”è™½Éµ…Ğ„ ‰É•ÅÕ¥É•‘•Á•¹‘•¹äí‘•Á•¹‘•¹å}¥‘õ€¥ÌÕ¹…Ù…¥±…‰±”ˆ¤°(€€€€€€€€€€€€€€€€€€€…™™•Ñ•‘}…ÍÍ•ÑÌèÙ•Œ…m…ÍÍ•Ñ}¥¹±½¹” ¤°‘•Á•¹‘•¹å}¥¹±½¹” ¥t°(€€€€€€€€€€€€€€€ô¤ì(€€€€€€€€€€€€€€€¥¹Ñ•¹ÑÌ¹ÁÕÍ ¡É¡¥Ñ•ÑÕÉ•%¹Ñ•¹Ğèé‰Õ¥± (€€€€€€€€€€€€€€€€€€€%¹Ñ•¹Ñ-¥¹èé	±½­AÉ½µ½Ñ¥½¸°(€€€€€€€€€€€€€€€€€€€…ÍÍ•Ñ}¥¹±½¹” ¤°(€€€€€€€€€€€€€€€€€€€Ù•Œ…m…ÍÍ•Ñ}¥¹±½¹” ¥t°(€€€€€€€€€€€€€€€€€€€	QÉ••M•Ğèé™É½´¡l‰‘•Á•¹‘•¹ä½ÕÑ…”•Ù¥‘•¹”…‘µ¥ÑÑ•ˆ¹Ñ½}ÍÑÉ¥¹œ ¥t¤°(€€€€€€€€€€€€€€€€€€€	QÉ••M•Ğèé™É½´¡l‰ÁÉ½µ½Ñ¥½¹}…Ñ”ˆ¹Ñ½}ÍÑÉ¥¹œ ¥t¤°(€€€€€€€€€€€€€€€€€€€	QÉ••M•Ğèé™É½´¡l‰ÁÉ½µ½Ñ¥½¹}É•™ÕÍ…±}É••¥ÁĞˆ¹Ñ½}ÍÑÉ¥¹œ ¥t¤°(€€€€€€€€€€€€€€€€€€€	QÉ••5…Àèé™É½´¡l ‰‘•Á•¹‘•¹å}¥ˆ¹Ñ½}ÍÑÉ¥¹œ ¤°‘•Á•¹‘•¹å}¥¹±½¹” ¤¥t¤°(€€€€€€€€€€€€€€€€¤ü¤ì(€€€€€€€€€€€€€€€¥¹Ñ•¹ÑÌ¹ÁÕÍ ¡É¡¥Ñ•ÑÕÉ•%¹Ñ•¹Ğèé‰Õ¥± (€€€€€€€€€€€€€€€€€€€%¹Ñ•¹Ñ-¥¹èéI•Á±…¹QÉ…¹Í¥Ñ¥½¸°(€€€€€€€€€€€€€€€€€€€…ÍÍ•Ñ}¥¹±½¹” ¤°(€€€€€€€€€€€€€€€€€€€Ù•Œ…m…ÍÍ•Ñ}¥¹±½¹” ¤°‘•Á•¹‘•¹å}¥¹±½¹” ¥t°(€€€€€€€€€€€€€€€€€€€	QÉ••M•Ğèé™É½´¡l‰…±Ñ•É¹…Ñ¥Ù”‰Õ¥±‘¥¹œ‰±½­Ì¥‘•¹Ñ¥™¥•ˆ¹Ñ½}ÍÑÉ¥¹œ ¥t¤°(€€€€€€€€€€€€€€€€€€€	QÉ••M•Ğèé™É½´¡l‰…É¡¥Ñ•ÑÕÉ•}Á±…¹¹•Èˆ¹Ñ½}ÍÑÉ¥¹œ ¥t¤°(€€€€€€€€€€€€€€€€€€€	QÉ••M•Ğèé™É½´¡l‰Á±…¹}•ÉÑ¥™¥…Ñ”ˆ¹Ñ½}ÍÑÉ¥¹œ ¥t¤°(€€€€€€€€€€€€€€€€€€€	QÉ••5…Àèé™É½´¡l ‰Õ¹…Ù…¥±…‰±•}‘•Á•¹‘•¹äˆ¹Ñ½}ÍÑÉ¥¹œ ¤°‘•Á•¹‘•¹å}¥¹±½¹” ¤¥t¤°(€€€€€€€€€€€€€€€€¤ü¤ì(€€€€€€€€€€€ô(€€€€€€€€€€€MÑ¥µÕ±ÕÌèé1¥™•å±••…‘±¥¹”ì…ÍÍ•Ñ}¥°Ñ…É•Ğô€ôøì(€€€€€€€€€€€€€€€±•Ğ…ÍÍ•Ğ€ôÍ•±˜¹ÍÑ…Ñ”¹É•¥ÍÑÉä¹…ÍÍ•Ğ¡…ÍÍ•Ñ}¥¤üì(€€€€€€€€€€€€€€€¥˜…ÍÍ•Ğ¹±¥™•å±”¹…±±½İÌ ©Ñ…É•Ğ¤ì(€€€€€€€€€€€€€€€€€€€‘¥…¹½Í•Ì¹ÁÕÍ ¡¥…¹½Í¥Ìì(€€€€€€€€€€€€€€€€€€€€€€€½‘”è€‰µUQ<´ÔÀÈˆ¹Ñ½}ÍÑÉ¥¹œ ¤°(€€€€€€€€€€€€€€€€€€€€€€€Í•Ù•É¥ÑäèM•Ù•É¥Ñäèé]…É¹¥¹œ°(€€€€€€€€€€€€€€€€€€€€€€€ÍÕ‰©•Ğè…ÍÍ•Ñ}¥¹±½¹” ¤°(€€€€€€€€€€€€€€€€€€€€€€€É…Ñ¥½¹…±”è™½Éµ…Ğ„ (€€€€€€€€€€€€€€€€€€€€€€€€€€€€‰±¥™•å±”‘•…‘±¥¹”É•ÅÕ•ÍÑÌíô€´øíôˆ°(€€€€€€€€€€€€€€€€€€€€€€€€€€€…ÍÍ•Ğ¹±¥™•å±”°Ñ…É•Ğ(€€€€€€€€€€€€€€€€€€€€€€€€¤°(€€€€€€€€€€€€€€€€€€€€€€€…™™•Ñ•‘}…ÍÍ•ÑÌèÙ•Œ…m…ÍÍ•Ñ}¥¹±½¹” ¥t°(€€€€€€€€€€€€€€€€€€€ô¤ì(€€€€€€€€€€€€€€€€€€€¥¹Ñ•¹ÑÌ¹ÁÕÍ ¡É¡¥Ñ•ÑÕÉ•%¹Ñ•¹Ğèé‰Õ¥± (€€€€€€€€€€€€€€€€€€€€€€€%¹Ñ•¹Ñ-¥¹èéÉ•…Ñ•5¥É…Ñ¥½¹A±…¸°(€€€€€€€€€€€€€€€€€€€€€€€…ÍÍ•Ñ}¥¹±½¹” ¤°(€€€€€€€€€€€€€€€€€€€€€€€Í•±˜¹ÍÑ…Ñ”¹É•¥ÍÑÉä¹¥µÁ…Ñ}É•Á½ÉĞ¡…ÍÍ•Ñ}¥¤ü¹…™™•Ñ•°(€€€€€€€€€€€€€€€€€€€€€€€	QÉ••M•Ğèé™É½´¡l(€€€€€€€€€€€€€€€€€€€€€€€€€€€€‰ÍÕ•ÍÍ½È¥‘•¹Ñ¥™¥•ˆ¹Ñ½}ÍÑÉ¥¹œ ¤°(€€€€€€€€€€€€€€€€€€€€€€€€€€€€‰É½±±‰…¬Á½±¥ä…‘µ¥ÑÑ•ˆ¹Ñ½}ÍÑÉ¥¹œ ¤°(€€€€€€€€€€€€€€€€€€€€€€€t¤°(€€€€€€€€€€€€€€€€€€€€€€€	QÉ••M•Ğèé™É½´¡l‰…É¡¥Ñ•ÑÕÉ•}Á±…¹¹•Èˆ¹Ñ½}ÍÑÉ¥¹œ ¥t¤°(€€€€€€€€€€€€€€€€€€€€€€€	QÉ••M•Ğèé™É½´¡l(€€€€€€€€€€€€€€€€€€€€€€€€€€€€‰µ¥É…Ñ¥½¹}Á±…¸ˆ¹Ñ½}ÍÑÉ¥¹œ ¤°(€€€€€€€€€€€€€€€€€€€€€€€€€€€€‰Á±…¹}•ÉÑ¥™¥…Ñ”ˆ¹Ñ½}ÍÑÉ¥¹œ ¤°(€€€€€€€€€€€€€€€€€€€€€€€t¤°(€€€€€€€€€€€€€€€€€€€€€€€	QÉ••5…Àèé™É½´¡l(€€€€€€€€€€€€€€€€€€€€€€€€€€€€ ‰™É½´ˆ¹Ñ½}ÍÑÉ¥¹œ ¤°…ÍÍ•Ğ¹±¥™•å±”¹Ñ½}ÍÑÉ¥¹œ ¤¤°(€€€€€€€€€€€€€€€€€€€€€€€€€€€€ ‰Ñ¼ˆ¹Ñ½}ÍÑÉ¥¹œ ¤°Ñ…É•Ğ¹Ñ½}ÍÑÉ¥¹œ ¤¤°(€€€€€€€€€€€€€€€€€€€€€€€t¤°(€€€€€€€€€€€€€€€€€€€€¤ü¤ì(€€€€€€€€€€€€€€€ô•±Í”ì(€€€€€€€€€€€€€€€€€€€‘¥…¹½Í•Ì¹ÁÕÍ ¡¥…¹½Í¥Ìì(€€€€€€€€€€€€€€€€€€€€€€€½‘”è€‰µUQ<´ÔÀÄˆ¹Ñ½}ÍÑÉ¥¹œ ¤°(€€€€€€€€€€€€€€€€€€€€€€€Í•Ù•É¥ÑäèM•Ù•É¥ÑäèéÉÉ½È°(€€€€€€€€€€€€€€€€€€€€€€€ÍÕ‰©•Ğè…ÍÍ•Ñ}¥¹±½¹” ¤°(€€€€€€€€€€€€€€€€€€€€€€€É…Ñ¥½¹…±”è™½Éµ…Ğ„ (€€€€€€€€€€€€€€€€€€€€€€€€€€€€‰É•ÅÕ•ÍÑ•±¥™•å±”ÑÉ…¹Í¥Ñ¥½¸íô€´øíô¥ÌÕ¹±…İ™Õ°ˆ°(€€€€€€€€€€€€€€€€€€€€€€€€€€€…ÍÍ•Ğ¹±¥™•å±”°Ñ…É•Ğ(€€€€€€€€€€€€€€€€€€€€€€€€¤°(€€€€€€€€€€€€€€€€€€€€€€€…™™•Ñ•‘}…ÍÍ•ÑÌèÙ•Œ…m…ÍÍ•Ñ}¥¹±½¹” ¥t°(€€€€€€€€€€€€€€€€€€€ô¤ì(€€€€€€€€€€€€€€€€€€€¥¹Ñ•¹ÑÌ¹ÁÕÍ ¡É¡¥Ñ•ÑÕÉ•%¹Ñ•¹Ğèé‰Õ¥± (€€€€€€€€€€€€€€€€€€€€€€€%¹Ñ•¹Ñ-¥¹èé	±½­AÉ½µ½Ñ¥½¸°(€€€€€€€€€€€€€€€€€€€€€€€…ÍÍ•Ñ}¥¹±½¹” ¤°(€€€€€€€€€€€€€€€€€€€€€€€Ù•Œ…m…ÍÍ•Ñ}¥¹±½¹” ¥t°(€€€€€€€€€€€€€€€€€€€€€€€	QÉ••M•Ğèé™É½´¡l‰±¥™•å±”±…Ü•Ù…±Õ…Ñ•ˆ¹Ñ½}ÍÑÉ¥¹œ ¥t¤°(€€€€€€€€€€€€€€€€€€€€€€€	QÉ••M•Ğèé™É½´¡l‰±¥™•å±•}…Ñ”ˆ¹Ñ½}ÍÑÉ¥¹œ ¥t¤°(€€€€€€€€€€€€€€€€€€€€€€€	QÉ••M•Ğèé™É½´¡l‰ÑÉ…¹Í¥Ñ¥½¹}É•™ÕÍ…±}É••¥ÁĞˆ¹Ñ½}ÍÑÉ¥¹œ ¥t¤°(€€€€€€€€€€€€€€€€€€€€€€€	QÉ••5…Àèé™É½´¡l(€€€€€€€€€€€€€€€€€€€€€€€€€€€€ ‰™É½´ˆ¹Ñ½}ÍÑÉ¥¹œ ¤°…ÍÍ•Ğ¹±¥™•å±”¹Ñ½}ÍÑÉ¥¹œ ¤¤°(€€€€€€€€€€€€€€€€€€€€€€€€€€€€ ‰Ñ¼ˆ¹Ñ½}ÍÑÉ¥¹œ ¤°Ñ…É•Ğ¹Ñ½}ÍÑÉ¥¹œ ¤¤°(€€€€€€€€€€€€€€€€€€€€€€€t¤°(€€€€€€€€€€€€€€€€€€€€¤ü¤ì(€€€€€€€€€€€€€€€ô(€€€€€€€€€€€ô(€€€€€€€€€€€MÑ¥µÕ±ÕÌèéMÑ…¹‘¥¹¡…¹•ì…ÍÍ•Ñ}¥°ÍÑ…¹‘¥¹œô€ôøì(€€€€€€€€€€€€€€€Í•±˜¹ÍÑ…Ñ”¹É•¥ÍÑÉä¹…ÍÍ•Ğ¡…ÍÍ•Ñ}¥¤üì(€€€€€€€€€€€€€€€±•ĞÍ•Ù•É¥Ñä€ôµ…Ñ ÍÑ…¹‘¥¹œì(€€€€€€€€€€€€€€€€€€€MÑ…¹‘¥¹œèé±¥Ù”€ôøM•Ù•É¥Ñäèé%¹™¼°(€€€€€€€€€€€€€€€€€€€MÑ…¹‘¥¹œèéA…ÉÑ¥…±±¥Ù”ğMÑ…¹‘¥¹œèéU¹­¹½İ¸€ôøM•Ù•É¥Ñäèé]…É¹¥¹œ°(€€€€€€€€€€€€€€€€€€€MÑ…¹‘¥¹œèé	±½­•(€€€€€€€€€€€€€€€€€€€ğMÑ…¹‘¥¹œèé	Õ¥±‘	É½­•¸(€€€€€€€€€€€€€€€€€€€ğMÑ…¹‘¥¹œèéU¹ÍÕÁÁ½ÉÑ•(€€€€€€€€€€€€€€€€€€€ğMÑ…¹‘¥¹œèéI•Ñ¥É•€ôøM•Ù•É¥ÑäèéÉÉ½È°(€€€€€€€€€€€€€€€ôì(€€€€€€€€€€€€€€€‘¥…¹½Í•Ì¹ÁÕÍ ¡¥…¹½Í¥Ìì(€€€€€€€€€€€€€€€€€€€½‘”è€‰µUQ<´ØÀÄˆ¹Ñ½}ÍÑÉ¥¹œ ¤°(€€€€€€€€€€€€€€€€€€€Í•Ù•É¥Ñä°(€€€€€€€€€€€€€€€€€€€ÍÕ‰©•Ğè…ÍÍ•Ñ}¥¹±½¹” ¤°(€€€€€€€€€€€€€€€€€€€É…Ñ¥½¹…±”è™½Éµ…Ğ„ ‰•Ù¥‘•¹”ÍÑ…¹‘¥¹œ¡…¹•Ñ¼íÍÑ…¹‘¥¹œèıôˆ¤°(€€€€€€€€€€€€€€€€€€€…™™•Ñ•‘}…ÍÍ•ÑÌèÙ•Œ…m…ÍÍ•Ñ}¥¹±½¹” ¥t°(€€€€€€€€€€€€€€€ô¤ì(€€€€€€€€€€€€€€€¥˜Í•Ù•É¥Ñä€øôM•Ù•É¥ÑäèéÉÉ½Èì(€€€€€€€€€€€€€€€€€€€¥¹Ñ•¹ÑÌ¹ÁÕÍ ¡É¡¥Ñ•ÑÕÉ•%¹Ñ•¹Ğèé‰Õ¥± (€€€€€€€€€€€€€€€€€€€€€€€%¹Ñ•¹Ñ-¥¹èé	±½­AÉ½µ½Ñ¥½¸°(€€€€€€€€€€€€€€€€€€€€€€€…ÍÍ•Ñ}¥¹±½¹” ¤°(€€€€€€€€€€€€€€€€€€€€€€€Í•±˜¹ÍÑ…Ñ”¹É•¥ÍÑÉä¹¥µÁ…Ñ}É•Á½ÉĞ¡…ÍÍ•Ñ}¥¤ü¹…™™•Ñ•°(€€€€€€€€€€€€€€€€€€€€€€€	QÉ••M•Ğèé™É½´¡l‰ÍÑ…¹‘¥¹œ½‰Í•ÉÙ…Ñ¥½¸…‘µ¥ÑÑ•ˆ¹Ñ½}ÍÑÉ¥¹œ ¥t¤°(€€€€€€€€€€€€€€€€€€€€€€€	QÉ••M•Ğèé™É½´¡l‰ÁÉ½µ½Ñ¥½¹}…Ñ”ˆ¹Ñ½}ÍÑÉ¥¹œ ¥t¤°(€€€€€€€€€€€€€€€€€€€€€€€	QÉ••M•Ğèé™É½´¡l‰ÁÉ½µ½Ñ¥½¹}É•™ÕÍ…±}É••¥ÁĞˆ¹Ñ½}ÍÑÉ¥¹œ ¥t¤°(€€€€€€€€€€€€€€€€€€€€€€€	QÉ••5…Àèé™É½´¡l ‰ÍÑ…¹‘¥¹œˆ¹Ñ½}ÍÑÉ¥¹œ ¤°™½Éµ…Ğ„ ‰íÍÑ…¹‘¥¹œèıôˆ¤¥t¤°(€€€€€€€€€€€€€€€€€€€€¤ü¤ì(€€€€€€€€€€€€€€€ô(€€€€€€€€€€€ô(€€€€€€€ô(€€€€€€€=¬  ¤¤(€€€ô((€€€™¸…Á…¥Ñå}¥¹Ñ•¹Ğ (€€€€€€€­¥¹è%¹Ñ•¹Ñ-¥¹°Í…µÁ±”è€™…Á…¥ÑåM…µÁ±”°±•Ù•°è…Á…¥Ñå1•Ù•°°(€€€€¤€´øI•ÍÕ±ĞñÉ¡¥Ñ•ÑÕÉ•%¹Ñ•¹Ğøì(€€€€€€€É¡¥Ñ•ÑÕÉ•%¹Ñ•¹Ğèé‰Õ¥± (€€€€€€€€€€€­¥¹°(€€€€€€€€€€€Í…µÁ±”¹±…‰•°¹±½¹” ¤°(€€€€€€€€€€€Y•Œèé¹•Ü ¤°(€€€€€€€€€€€	QÉ••M•Ğèé™É½´¡l‰…Á…¥ÑäÍ…µÁ±”…‘µ¥ÑÑ•ˆ¹Ñ½}ÍÑÉ¥¹œ ¥t¤°(€€€€€€€€€€€	QÉ••M•Ğèé™É½´¡mµ…Ñ ­¥¹ì(€€€€€€€€€€€€€€€%¹Ñ•¹Ñ-¥¹èé	±½­AÉ½µ½Ñ¥½¸€ôø€‰ÁÉ½µ½Ñ¥½¹}…Ñ”ˆ¹Ñ½}ÍÑÉ¥¹œ ¤°(€€€€€€€€€€€€€€€|€ôø€‰…É¡¥Ñ•ÑÕÉ•}ÁÉ½™¥±•Èˆ¹Ñ½}ÍÑÉ¥¹œ ¤°(€€€€€€€€€€€õt¤°(€€€€€€€€€€€	QÉ••M•Ğèé™É½´¡l(€€€€€€€€€€€€€€€€‰…Á…¥Ñå}É•Á½ÉĞˆ¹Ñ½}ÍÑÉ¥¹œ ¤°(€€€€€€€€€€€€€€€€‰…Á…¥Ñå}É••¥ÁĞˆ¹Ñ½}ÍÑÉ¥¹œ ¤°(€€€€€€€€€€€t¤°(€€€€€€€€€€€	QÉ••5…Àèé™É½´¡l(€€€€€€€€€€€€€€€€ ‰…Á…¥Ñå}±•Ù•°ˆ¹Ñ½}ÍÑÉ¥¹œ ¤°™½Éµ…Ğ„ ‰í±•Ù•°èıôˆ¤¤°(€€€€€€€€€€€€€€€€ ‰•±…ÁÍ•‘}µÌˆ¹Ñ½}ÍÑÉ¥¹œ ¤°Í…µÁ±”¹•±…ÁÍ•‘}µÌ¹Ñ½}ÍÑÉ¥¹œ ¤¤°(€€€€€€€€€€€€€€€€ (€€€€€€€€€€€€€€€€€€€€‰Á•…­}µ•µ½Éå}‰åÑ•Ìˆ¹Ñ½}ÍÑÉ¥¹œ ¤°(€€€€€€€€€€€€€€€€€€€Í…µÁ±”¹Á•…­}µ•µ½Éå}‰åÑ•Ì¹Ñ½}ÍÑÉ¥¹œ ¤°(€€€€€€€€€€€€€€€€¤°(€€€€€€€€€€€€€€€€ (€€€€€€€€€€€€€€€€€€€€‰İ½É­±½…‘}Õ¹¥ÑÌˆ¹Ñ½}ÍÑÉ¥¹œ ¤°(€€€€€€€€€€€€€€€€€€€Í…µÁ±”¹İ½É­±½…¹Õ¹¥ÑÌ ¤¹Ñ½}ÍÑÉ¥¹œ ¤°(€€€€€€€€€€€€€€€€¤°(€€€€€€€€€€€t¤°(€€€€€€€€¤(€€€ô)ô(