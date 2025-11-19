//! Root Cause Analysis at Generation Site
//!
//! Implements lean problem-solving techniques:
//! - 5 Whys: Iterative questioning to find root cause
//! - Fishbone Diagram (Ishikawa): Categorized cause analysis
//! - A3 Problem Solving: Structured problem-solving format

use super::*;
use std::collections::BTreeMap;

/// Root cause analyzer for generation problems
pub struct RootCauseAnalyzer {
    /// Problem being analyzed
    problem: Problem,

    /// 5 Whys analysis
    five_whys: Option<FiveWhys>,

    /// Fishbone diagram
    fishbone: Option<FishboneDiagram>,

    /// A3 analysis
    a3: Option<A3Analysis>,
}

/// A problem that needs root cause analysis
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Problem {
    /// Problem ID
    pub id: String,

    /// Problem description
    pub description: String,

    /// Where the problem occurred
    pub location: GenerationSite,

    /// When the problem occurred
    pub occurred_at: String,

    /// Impact severity (1-10)
    pub severity: u8,

    /// Frequency of occurrence
    pub frequency: Frequency,

    /// Current status
    pub status: ProblemStatus,
}

/// Status of a problem
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub enum ProblemStatus {
    /// Problem identified but not analyzed
    Identified,

    /// Root cause analysis in progress
    Analyzing,

    /// Root cause identified
    RootCauseFound,

    /// Countermeasures being developed
    DevelopingCountermeasures,

    /// Countermeasures being implemented
    Implementing,

    /// Countermeasures implemented, monitoring effectiveness
    Monitoring,

    /// Problem resolved
    Resolved,

    /// Problem could not be resolved
    Unresolved,
}

/// 5 Whys analysis
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct FiveWhys {
    /// The initial problem statement
    pub problem: String,

    /// Chain of "why" questions and answers
    pub whys: Vec<Why>,

    /// Root cause identified
    pub root_cause: Option<String>,

    /// Confidence in root cause (0.0-1.0)
    pub confidence: f64,
}

/// A single "why" question and answer
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Why {
    /// Question number (1-5+)
    pub number: usize,

    /// The "why" question
    pub question: String,

    /// The answer
    pub answer: String,

    /// Whether this answer leads to actionable countermeasures
    pub is_actionable: bool,
}

/// Fishbone diagram for categorized cause analysis
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct FishboneDiagram {
    /// The effect/problem being analyzed
    pub effect: String,

    /// Causes organized by category (6Ms)
    pub causes: BTreeMap<CauseCategory, Vec<Cause>>,

    /// Root causes identified
    pub root_causes: Vec<String>,
}

/// Fishbone cause categories (6Ms)
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq, PartialOrd, Ord)]
pub enum CauseCategory {
    /// Man/People: Human factors
    Man,

    /// Machine: Technology, tools, systems
    Machine,

    /// Method: Processes, procedures
    Method,

    /// Material: Inputs, data, dependencies
    Material,

    /// Measurement: Metrics, monitoring
    Measurement,

    /// Mother Nature: Environment, external factors
    MotherNature,
}

/// A cause in the fishbone diagram
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Cause {
    /// Cause description
    pub description: String,

    /// Sub-causes
    pub sub_causes: Vec<String>,

    /// Likelihood this is the root cause (0.0-1.0)
    pub likelihood: f64,

    /// Whether this cause has been verified
    pub verified: bool,
}

/// A3 Problem Solving format
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct A3Analysis {
    /// Title/Theme
    pub title: String,

    /// Background context
    pub background: String,

    /// Current condition
    pub current_condition: CurrentCondition,

    /// Goal/Target condition
    pub goal: String,

    /// Root cause analysis results
    pub root_cause_analysis: String,

    /// Countermeasures
    pub countermeasures: Vec<Countermeasure>,

    /// Implementation plan
    pub implementation_plan: Vec<ImplementationStep>,

    /// Follow-up plan
    pub follow_up: FollowUpPlan,
}

/// Current condition in A3
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct CurrentCondition {
    /// Description
    pub description: String,

    /// Key metrics
    pub metrics: BTreeMap<String, f64>,

    /// Visual data (charts, graphs)
    pub visuals: Vec<String>,
}

/// A countermeasure to address root cause
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Countermeasure {
    /// Countermeasure ID
    pub id: String,

    /// Description
    pub description: String,

    /// Root cause this addresses
    pub addresses_root_cause: String,

    /// Expected impact
    pub expected_impact: String,

    /// Owner responsible
    pub owner: String,

    /// Due date
    pub due_date: String,

    /// Status
    pub status: CountermeasureStatus,
}

/// Status of a countermeasure
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub enum CountermeasureStatus {
    /// Planned but not started
    Planned,

    /// In progress
    InProgress,

    /// Completed
    Completed,

    /// On hold
    OnHold,

    /// Cancelled
    Cancelled,
}

/// Implementation step for countermeasure
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ImplementationStep {
    /// Step number
    pub step: usize,

    /// Action to take
    pub action: String,

    /// Who is responsible
    pub owner: String,

    /// When it should be done
    pub due_date: String,

    /// Completion status
    pub completed: bool,
}

/// Follow-up plan for monitoring
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct FollowUpPlan {
    /// Metrics to monitor
    pub metrics_to_monitor: Vec<String>,

    /// Check frequency
    pub check_frequency: String,

    /// Target values
    pub targets: BTreeMap<String, f64>,

    /// Review date
    pub review_date: String,
}

impl RootCauseAnalyzer {
    /// Create new root cause analyzer for a problem
    pub fn new(problem: Problem) -> Self {
        Self {
            problem,
            five_whys: None,
            fishbone: None,
            a3: None,
        }
    }

    /// Perform 5 Whys analysis
    pub fn perform_five_whys(&mut self, whys: Vec<(String, String)>) -> &FiveWhys {
        let mut why_chain = Vec::new();
        let mut is_actionable = false;

        for (i, (question, answer)) in whys.iter().enumerate() {
            // Check if this answer is actionable (contains specific, controllable cause)
            is_actionable = Self::is_actionable_answer(answer);

            why_chain.push(Why {
                number: i + 1,
                question: question.clone(),
                answer: answer.clone(),
                is_actionable,
            });

            // Stop if we found an actionable root cause
            if is_actionable {
                break;
            }
        }

        // Last actionable answer is the root cause
        let root_cause = why_chain
            .iter()
            .rev()
            .find(|w| w.is_actionable)
            .map(|w| w.answer.clone());

        // Confidence based on depth and actionability
        let confidence = if root_cause.is_some() {
            // Higher confidence with more whys (up to 5)
            let depth_factor = (why_chain.len() as f64 / 5.0).min(1.0);
            depth_factor * 0.8 + 0.2 // Range: 0.2 to 1.0
        } else {
            0.3 // Low confidence if no actionable root cause found
        };

        self.five_whys = Some(FiveWhys {
            problem: self.problem.description.clone(),
            whys: why_chain,
            root_cause,
            confidence,
        });

        self.five_whys.as_ref().unwrap()
    }

    /// Check if an answer is actionable (specific and controllable)
    fn is_actionable_answer(answer: &str) -> bool {
        let answer_lower = answer.to_lowercase();

        // Actionable answers typically contain specific technical details
        let actionable_keywords = [
            "missing", "incorrect", "not implemented", "bug", "config", "setting", "parameter",
            "version", "dependency", "cache", "timeout", "permission", "validation",
        ];

        // Non-actionable answers are too vague
        let vague_keywords = ["unknown", "unclear", "maybe", "possibly", "might"];

        let has_actionable = actionable_keywords
            .iter()
            .any(|k| answer_lower.contains(k));
        let has_vague = vague_keywords.iter().any(|k| answer_lower.contains(k));

        has_actionable && !has_vague
    }

    /// Build fishbone diagram
    pub fn build_fishbone(&mut self, effect: String) -> &mut FishboneDiagram {
        let mut causes = BTreeMap::new();

        // Initialize empty categories
        causes.insert(CauseCategory::Man, Vec::new());
        causes.insert(CauseCategory::Machine, Vec::new());
        causes.insert(CauseCategory::Method, Vec::new());
        causes.insert(CauseCategory::Material, Vec::new());
        causes.insert(CauseCategory::Measurement, Vec::new());
        causes.insert(CauseCategory::MotherNature, Vec::new());

        self.fishbone = Some(FishboneDiagram {
            effect,
            causes,
            root_causes: Vec::new(),
        });

        self.fishbone.as_mut().unwrap()
    }

    /// Add cause to fishbone diagram
    pub fn add_cause(
        &mut self,
        category: CauseCategory,
        description: String,
        sub_causes: Vec<String>,
        likelihood: f64,
    ) {
        if let Some(fishbone) = &mut self.fishbone {
            let cause = Cause {
                description,
                sub_causes,
                likelihood,
                verified: false,
            };

            fishbone.causes.entry(category).or_default().push(cause);
        }
    }

    /// Identify root causes from fishbone
    pub fn identify_root_causes(&mut self, threshold: f64) {
        if let Some(fishbone) = &mut self.fishbone {
            let mut root_causes = Vec::new();

            for causes in fishbone.causes.values() {
                for cause in causes {
                    if cause.likelihood >= threshold && cause.verified {
                        root_causes.push(cause.description.clone());
                    }
                }
            }

            fishbone.root_causes = root_causes;
        }
    }

    /// Start A3 analysis
    pub fn start_a3(&mut self, title: String, background: String, goal: String) {
        self.a3 = Some(A3Analysis {
            title,
            background,
            current_condition: CurrentCondition {
                description: self.problem.description.clone(),
                metrics: BTreeMap::new(),
                visuals: Vec::new(),
            },
            goal,
            root_cause_analysis: String::new(),
            countermeasures: Vec::new(),
            implementation_plan: Vec::new(),
            follow_up: FollowUpPlan {
                metrics_to_monitor: Vec::new(),
                check_frequency: "weekly".to_string(),
                targets: BTreeMap::new(),
                review_date: String::new(),
            },
        });
    }

    /// Add countermeasure to A3
    pub fn add_countermeasure(&mut self, countermeasure: Countermeasure) {
        if let Some(a3) = &mut self.a3 {
            a3.countermeasures.push(countermeasure);
        }
    }

    /// Get analysis results
    pub fn get_results(&self) -> RootCauseResults {
        RootCauseResults {
            problem: self.problem.clone(),
            five_whys: self.five_whys.clone(),
            fishbone: self.fishbone.clone(),
            a3: self.a3.clone(),
        }
    }
}

/// Complete root cause analysis results
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct RootCauseResults {
    /// The problem analyzed
    pub problem: Problem,

    /// 5 Whys results
    pub five_whys: Option<FiveWhys>,

    /// Fishbone diagram
    pub fishbone: Option<FishboneDiagram>,

    /// A3 analysis
    pub a3: Option<A3Analysis>,
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_five_whys() {
        let problem = Problem {
            id: "p1".to_string(),
            description: "Template rendering failed".to_string(),
            location: GenerationSite {
                template_path: "test.tmpl".to_string(),
                output_path: "test.rs".to_string(),
                phase: PipelinePhase::BodyRendering,
                graph_context: None,
                sparql_queries: vec![],
            },
            occurred_at: chrono::Utc::now().to_rfc3339(),
            severity: 8,
            frequency: Frequency::Frequent,
            status: ProblemStatus::Identified,
        };

        let mut analyzer = RootCauseAnalyzer::new(problem);

        let whys = vec![
            ("Why did rendering fail?".to_string(), "Variable 'name' was undefined".to_string()),
            ("Why was the variable undefined?".to_string(), "SPARQL query returned no results".to_string()),
            ("Why did the query return no results?".to_string(), "RDF graph was missing data for entity".to_string()),
            ("Why was the data missing?".to_string(), "Ontology file not loaded due to incorrect path".to_string()),
        ];

        let result = analyzer.perform_five_whys(whys);

        assert!(result.root_cause.is_some());
        assert!(result.confidence > 0.5);
        assert_eq!(result.whys.len(), 4);
    }

    #[test]
    fn test_fishbone() {
        let problem = Problem {
            id: "p1".to_string(),
            description: "Test".to_string(),
            location: GenerationSite {
                template_path: "test".to_string(),
                output_path: "test".to_string(),
                phase: PipelinePhase::TemplateLoading,
                graph_context: None,
                sparql_queries: vec![],
            },
            occurred_at: chrono::Utc::now().to_rfc3339(),
            severity: 5,
            frequency: Frequency::Occasional,
            status: ProblemStatus::Identified,
        };

        let mut analyzer = RootCauseAnalyzer::new(problem);

        analyzer.build_fishbone("Slow generation".to_string());
        analyzer.add_cause(
            CauseCategory::Machine,
            "No caching".to_string(),
            vec!["SPARQL queries not cached".to_string()],
            0.8,
        );

        let fishbone = analyzer.fishbone.as_ref().unwrap();
        assert_eq!(fishbone.effect, "Slow generation");
        assert_eq!(fishbone.causes[&CauseCategory::Machine].len(), 1);
    }
}
