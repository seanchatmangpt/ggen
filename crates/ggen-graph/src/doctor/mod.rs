use crate::graph::DeterministicGraph;
use crate::GraphError;

pub struct Deviation {
    pub description: String,
}

pub struct DiagnoseReport {
    pub conforms: bool,
    pub fitness: f64,
    pub deviations: Vec<Deviation>,
}

pub struct ProcessDoctor;

impl ProcessDoctor {
    pub fn diagnose(
        graph: &DeterministicGraph, expected_seq: &[String],
    ) -> Result<DiagnoseReport, GraphError> {
        let ocel_log = crate::ocel::EvidenceProjector::extract_ocel(graph)?;
        let actual_activities: Vec<String> =
            ocel_log.events.iter().map(|e| e.activity.clone()).collect();

        if actual_activities == expected_seq {
            Ok(DiagnoseReport {
                conforms: true,
                fitness: 1.0,
                deviations: Vec::new(),
            })
        } else {
            let mut expected_idx = 0;
            let mut actual_idx = 0;
            let mut skipped = Vec::new();
            while expected_idx < expected_seq.len() {
                if actual_idx < actual_activities.len()
                    && actual_activities[actual_idx] == expected_seq[expected_idx]
                {
                    actual_idx += 1;
                } else {
                    skipped.push(expected_seq[expected_idx].clone());
                }
                expected_idx += 1;
            }

            let mut deviations = Vec::new();
            for sk in skipped {
                deviations.push(Deviation {
                    description: format!("Skipped stage: {}", sk),
                });
            }

            Ok(DiagnoseReport {
                conforms: false,
                fitness: 0.0,
                deviations,
            })
        }
    }
}
