const WIDTH: usize = 8;
const DIMENSIONS: usize = 4;
const CELL_COUNT: usize = WIDTH * WIDTH * WIDTH * WIDTH;
const LANES: usize = 8;
const BROKER_AUTHORITY: u64 = 1 << 8;

const AUTHORITY_LABELS: [&str; WIDTH] = [
    "none", "a", "b", "a+b", "c", "a+c", "b+c", "a+b+c",
];
const READINESS_LABELS: [&str; WIDTH] = [
    "none", "r1", "r2", "r1+r2", "r3", "r1+r3", "r2+r3", "r1+r2+r3",
];
const TIME_BUDGETS: [u32; WIDTH] = [0, 1, 5, 10, 20, 50, 100, 200];
const MODE_LABELS: [&str; WIDTH] = [
    "base-forward",
    "deterministic-forward",
    "receipt-forward",
    "deterministic-receipt-forward",
    "base-reverse-tie",
    "deterministic-reverse-tie",
    "receipt-reverse-tie",
    "deterministic-receipt-reverse-tie",
];

const CONTRACTS: &[TestContract] = &[
    TestContract::new(
        "tcps-8pow4/selection-matrix",
        &["tcps.auto-select", "tcps.8pow4.matrix"],
        &[
            "hard-gates-before-mass",
            "maximum-ready-mass",
            "deterministic-tie-break",
            "typed-refusal",
            "receipt-semantic-identity",
        ],
        ResourceEnvelope::new(u64::MAX, 128 * 1024 * 1024, false, false, false),
        &[],
    ),
    TestContract::new(
        "tcps-8pow4/authorization-boundary",
        &["tcps.blue-river-dam"],
        &[
            "selection-not-authorization",
            "tool-bound-capability",
            "authority-bound-capability",
            "expiry-bound-capability",
            "policy-bound-capability",
        ],
        ResourceEnvelope::new(u64::MAX, 16 * 1024 * 1024, false, false, false),
        &[],
    ),
    TestContract::new(
        "tcps-8pow4/evidence",
        &["tcps.8pow4.receipt"],
        &[
            "cryptographic-merkle-root",
            "replay-stable",
            "mutation-sensitive",
            "deterministic-sharding",
        ],
        ResourceEnvelope::new(u64::MAX, 128 * 1024 * 1024, false, true, false),
        &["real-filesystem"],
    ),
];

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Serialize)]
struct Coordinates {
    authority: u8,
    readiness: u8,
    time_budget: u8,
    mode: u8,
}

impl Coordinates {
    fn validate(self) -> Result<(), EvidenceError> {
        for (name, value) in [
            ("authority", self.authority),
            ("readiness", self.readiness),
            ("time-budget", self.time_budget),
            ("mode", self.mode),
        ] {
            if usize::from(value) >= WIDTH {
                return Err(EvidenceError::InvalidData {
                    identity: "tcps-8pow4-coordinate".to_owned(),
                    message: format!("INVALID_AXIS_STATE:{name}:{value}"),
                });
            }
        }
        Ok(())
    }

    const fn packed(self) -> u16 {
        self.authority as u16
            | ((self.readiness as u16) << 3)
            | ((self.time_budget as u16) << 6)
            | ((self.mode as u16) << 9)
    }

    fn identity(self) -> String {
        format!(
            "h4:{}:{}:t{}:{}",
            AUTHORITY_LABELS[usize::from(self.authority)],
            READINESS_LABELS[usize::from(self.readiness)],
            TIME_BUDGETS[usize::from(self.time_budget)],
            MODE_LABELS[usize::from(self.mode)],
        )
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
struct Cell {
    ordinal: u16,
    gray: u16,
    coordinates: Coordinates,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
struct RequestMode {
    deterministic_required: bool,
    receipt_required: bool,
    reverse_tie_policy: bool,
}

impl RequestMode {
    const fn from_state(state: u8) -> Self {
        Self {
            deterministic_required: state & 1 != 0,
            receipt_required: state & 2 != 0,
            reverse_tie_policy: state & 4 != 0,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum ExpectedOutcome {
    Selected {
        index: usize,
        mass: u16,
        eligible_mask: u64,
        ready_mask: u64,
    },
    Refused {
        reason: 拒否理由,
        eligible_mask: u64,
        ready_mask: u64,
    },
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize)]
#[serde(rename_all = "kebab-case")]
enum OutcomeClass {
    Selected,
    RefusedNoEligible,
    RefusedNoReady,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
struct ScenarioEvidence {
    ordinal: u16,
    gray: u16,
    identity: String,
    coordinates: Coordinates,
    outcome: OutcomeClass,
    eligible_mask: u64,
    ready_mask: u64,
    selected_tool: Option<u16>,
    selected_mass: Option<u16>,
    selection_receipt: String,
    authorization_receipt: Option<String>,
    evidence_digest: String,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
struct MatrixReceipt {
    schema: &'static str,
    cells: usize,
    selected: usize,
    refused_no_eligible: usize,
    refused_no_ready: usize,
    authorization_receipts: usize,
    merkle_root: String,
}
