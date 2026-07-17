#![allow(missing_docs)]
#![allow(dead_code)]
use crate::core::governance::RunId;
use crate::observability::ocel::collector::OcelCollector;
use crate::observability::ocel::types::{OcelLog, TestOcelEvent};
use wasm4pm_compat::admission::Admission;
use wasm4pm_compat::{Admitted, Evidence, Raw, Receipted, Witness, WitnessFamily};

pub struct TestSuiteWitness;

impl Witness for TestSuiteWitness {
    const KEY: &'static str = "test-suite";
    const FAMILY: WitnessFamily = WitnessFamily::Standard;
    const TITLE: &'static str = "Chicago TDD Test Suite";
    const YEAR: Option<u16> = None;
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum TestEventRefusal {
    MissingCaseId,
    NonMonotonicTimestamp,
    DanglingObjectReference,
}

impl OcelCollector {
    pub(crate) fn admit_event(
        &self,
        raw: &Evidence<TestOcelEvent, Raw, TestSuiteWitness>,
    ) -> Result<Evidence<TestOcelEvent, Admitted, TestSuiteWitness>, TestEventRefusal> {
        let event = raw.inner();

        if event.case_id.is_empty() {
            return Err(TestEventRefusal::MissingCaseId);
        }

        if let Some(last_ts) = self.last_timestamps.get(&event.case_id) {
            if event.timestamp_ns <= *last_ts {
                return Err(TestEventRefusal::NonMonotonicTimestamp);
            }
        }
        self.last_timestamps.insert(event.case_id.clone(), event.timestamp_ns);

        for (obj_id, _) in &event.objects {
            if !self.known_objects.contains(obj_id) {
                return Err(TestEventRefusal::DanglingObjectReference);
            }
        }

        Ok(Admission::<_, TestSuiteWitness>::new(event.clone()).into_evidence())
    }
}

/// Seals a run and generates receipted Evidence for the OCEL log, along with
/// a hex-encoded digest string.
///
/// # Errors
/// Returns an error if the log cannot be sealed.
pub fn seal_run(
    collector: &OcelCollector,
    _run_id: RunId,
) -> Result<(Evidence<OcelLog, Receipted, TestSuiteWitness>, String), String> {
    let mut log = OcelLog::new();
    {
        let events_guard = collector.events.lock().map_err(|e| e.to_string())?;
        for (i, ev) in events_guard.iter().enumerate() {
            let id = format!("evt_{i:03}");
            let _ = log.events.insert(id, ev.inner().clone());
        }
    }

    // Determinism: `log.events` is a HashMap, whose iteration order is unspecified.
    // Receipt material must be sorted before hashing (canonical order by event id),
    // otherwise two identical runs produce different digests.
    // Complexity: O(n log n) in event count for the sort.
    let mut sorted_events: Vec<(&String, &TestOcelEvent)> = log.events.iter().collect();
    sorted_events.sort_by_key(|(id, _)| id.as_str());

    let mut hasher = blake3::Hasher::new();
    for (id, ev) in sorted_events {
        hasher.update(id.as_bytes());
        hasher.update(ev.case_id.as_bytes());
        hasher.update(&ev.timestamp_ns.to_le_bytes());
        hasher.update(format!("{:?}", ev.activity).as_bytes());
        for (obj_id, obj_type) in &ev.objects {
            hasher.update(obj_id.as_bytes());
            hasher.update(format!("{obj_type:?}").as_bytes());
        }
    }

    let digest_bytes: [u8; 32] = *hasher.finalize().as_bytes();
    let digest_hex = digest_bytes.iter().fold(String::with_capacity(64), |mut acc, b| {
        use std::fmt::Write as _;
        let _ = write!(acc, "{b:02x}");
        acc
    });

    let admitted = Admission::<_, TestSuiteWitness>::new(log).into_evidence();
    let receipted = admitted.into_receipted();
    Ok((receipted, digest_hex))
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::observability::ocel::types::TestActivity;
    use std::collections::HashMap;

    fn synthetic_event(n: u64) -> TestOcelEvent {
        TestOcelEvent {
            event_id: format!("fixed-event-{n}"),
            case_id: "run-determinism".to_string(),
            activity: TestActivity::TestStarted,
            timestamp_ns: 1_000 + n,
            objects: vec![(
                "artifact-1".to_string(),
                crate::observability::ocel::types::TestObjectType::Artifact,
            )],
            attributes: HashMap::new(),
        }
    }

    fn collector_with_two_events() -> OcelCollector {
        let collector = OcelCollector::new(None);
        collector.known_objects.insert("artifact-1".to_string());
        {
            let mut events = collector.events.lock().unwrap();
            for n in 0..2 {
                events.push(
                    Admission::<_, TestSuiteWitness>::new(synthetic_event(n)).into_evidence(),
                );
            }
        }
        collector
    }

    /// Regression: identical logs must produce byte-identical digests
    /// (sorted-before-hashed; HashMap iteration order must not leak into the receipt).
    #[test]
    fn seal_run_digest_is_deterministic_for_identical_logs() {
        let a = collector_with_two_events();
        let b = collector_with_two_events();
        let (_, digest_a) = seal_run(&a, "run-determinism".to_string()).unwrap();
        let (_, digest_b) = seal_run(&b, "run-determinism".to_string()).unwrap();
        assert_eq!(digest_a, digest_b, "identical logs produced different digests");
    }

    /// Known-answer digest for a fixed synthetic 2-event log. If this changes,
    /// the receipt material or its canonical ordering changed — that is a
    /// breaking change to receipts and must be intentional.
    #[test]
    fn seal_run_digest_known_answer() {
        let collector = collector_with_two_events();
        let (_, digest) = seal_run(&collector, "run-determinism".to_string()).unwrap();
        assert_eq!(digest, KNOWN_ANSWER_DIGEST, "known-answer digest drifted");
    }

    /// Computed once from the fixed synthetic log above (blake3, sorted event ids).
    const KNOWN_ANSWER_DIGEST: &str =
        "6004b82b417f23a0de9dc04d0035f442653850f88e5ef43ccf21d4266703d25d";
}
