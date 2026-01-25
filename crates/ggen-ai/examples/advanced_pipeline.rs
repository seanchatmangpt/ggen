//! Example: Advanced Pipeline Composition
//!
//! Demonstrates composing multiple DSPy modules into complex pipelines:
//! 1. Sequential composition
//! 2. Parallel composition
//! 3. Conditional composition
//! 4. Multi-Hop QA pattern (simplified)
//!
//! Run with: cargo run --example advanced_pipeline
//!
//! Prerequisites:
//! - Set GGEN_LLM_MODEL environment variable
//! - Set appropriate API key

use async_trait::async_trait;
use ggen_ai::dspy::{
    field::{InputField, OutputField},
    module::{Module, ModuleError, ModuleResult},
    predictor::{ChainOfThought, Predictor},
    signature::Signature,
};
use serde_json::{json, Value};
use std::collections::HashMap;

/// Sequential Pipeline: Extract → Summarize
struct SequentialPipeline {
    extractor: Predictor,
    summarizer: Predictor,
    signature: Signature,
}

impl SequentialPipeline {
    fn new() -> Self {
        let extract_sig = Signature::new("EntityExtraction", "Extract key entities from text")
            .with_input(InputField::new("text", "Input text", "String"))
            .with_output(OutputField::new("entities", "List of entities", "String"))
            .with_instructions("Extract all named entities (people, places, organizations).");

        let summary_sig = Signature::new("Summarization", "Summarize extracted entities")
            .with_input(InputField::new("entities", "Entities", "String"))
            .with_output(OutputField::new("summary", "Summary", "String"))
            .with_instructions("Summarize the entities in 1-2 sentences.");

        Self {
            extractor: Predictor::new(extract_sig),
            summarizer: Predictor::new(summary_sig),
            signature: Signature::new("SequentialPipeline", "Extract and summarize"),
        }
    }
}

#[async_trait]
impl Module for SequentialPipeline {
    fn signature(&self) -> &Signature {
        &self.signature
    }

    async fn forward(
        &self, inputs: HashMap<String, Value>,
    ) -> ModuleResult<HashMap<String, Value>> {
        // Step 1: Extract entities
        let entities_result = self.extractor.forward(inputs).await?;

        // Step 2: Summarize (uses output from step 1)
        self.summarizer.forward(entities_result).await
    }
}

/// Parallel Pipeline: Multiple perspectives analyzed in parallel
struct ParallelPipeline {
    analyzers: Vec<Predictor>,
    signature: Signature,
}

impl ParallelPipeline {
    fn new() -> Self {
        // Create multiple analyzers with different perspectives
        let perspectives = vec![
            ("technical", "Analyze from a technical perspective"),
            ("business", "Analyze from a business perspective"),
            ("user", "Analyze from a user perspective"),
        ];

        let analyzers = perspectives
            .into_iter()
            .map(|(name, instructions)| {
                let sig =
                    Signature::new(&format!("{}Analysis", name), &format!("{} analysis", name))
                        .with_input(InputField::new("text", "Text to analyze", "String"))
                        .with_output(OutputField::new(
                            "analysis",
                            &format!("{} analysis", name),
                            "String",
                        ))
                        .with_instructions(instructions);

                Predictor::new(sig)
            })
            .collect();

        Self {
            analyzers,
            signature: Signature::new("ParallelPipeline", "Multi-perspective analysis"),
        }
    }
}

#[async_trait]
impl Module for ParallelPipeline {
    fn signature(&self) -> &Signature {
        &self.signature
    }

    async fn forward(
        &self, inputs: HashMap<String, Value>,
    ) -> ModuleResult<HashMap<String, Value>> {
        // Run all analyzers in parallel
        let futures: Vec<_> = self
            .analyzers
            .iter()
            .enumerate()
            .map(|(i, analyzer)| async move {
                let result = analyzer.forward(inputs.clone()).await?;
                Ok::<_, ModuleError>((i, result))
            })
            .collect();

        // Wait for all to complete
        let results = futures::future::try_join_all(futures).await?;

        // Aggregate results
        let mut output = HashMap::new();
        let mut all_analyses = Vec::new();

        for (_i, result) in results {
            if let Some(analysis) = result.get("analysis") {
                all_analyses.push(analysis.clone());
            }
        }

        output.insert("analyses".to_string(), json!(all_analyses));

        Ok(output)
    }
}

/// Conditional Pipeline: Route based on classification
struct ConditionalPipeline {
    classifier: Predictor,
    simple_handler: Predictor,
    complex_handler: ChainOfThought,
    signature: Signature,
}

impl ConditionalPipeline {
    fn new() -> Self {
        let classifier_sig = Signature::new("ComplexityClassifier", "Classify question complexity")
            .with_input(InputField::new("question", "Question", "String"))
            .with_output(OutputField::new(
                "complexity",
                "Complexity (simple/complex)",
                "String",
            ))
            .with_instructions("Classify as 'simple' or 'complex'.");

        let simple_sig = Signature::new("SimpleAnswer", "Answer simple questions")
            .with_input(InputField::new("question", "Question", "String"))
            .with_output(OutputField::new("answer", "Answer", "String"))
            .with_instructions("Provide a brief, direct answer.");

        let complex_sig = Signature::new("ComplexAnswer", "Answer complex questions")
            .with_input(InputField::new("question", "Question", "String"))
            .with_output(OutputField::new("reasoning", "Reasoning", "String"))
            .with_output(OutputField::new("answer", "Answer", "String"))
            .with_instructions("Think step-by-step and provide a detailed answer.");

        Self {
            classifier: Predictor::new(classifier_sig),
            simple_handler: Predictor::new(simple_sig),
            complex_handler: ChainOfThought::new(complex_sig),
            signature: Signature::new("ConditionalPipeline", "Adaptive QA"),
        }
    }
}

#[async_trait]
impl Module for ConditionalPipeline {
    fn signature(&self) -> &Signature {
        &self.signature
    }

    async fn forward(
        &self, inputs: HashMap<String, Value>,
    ) -> ModuleResult<HashMap<String, Value>> {
        // Classify input
        let classification = self.classifier.forward(inputs.clone()).await?;

        // Route based on classification
        let complexity = classification
            .get("complexity")
            .and_then(|v| v.as_str())
            .ok_or_else(|| ModuleError::Other("Missing complexity".to_string()))?;

        match complexity.to_lowercase().as_str() {
            "simple" => self.simple_handler.forward(inputs).await,
            "complex" => self.complex_handler.forward(inputs).await,
            _ => {
                // Default to complex handler for unknown
                self.complex_handler.forward(inputs).await
            }
        }
    }
}

/// Simplified Multi-Hop QA (without retrieval)
struct MultiHopQA {
    decomposer: ChainOfThought,
    answerer: ChainOfThought,
    signature: Signature,
}

impl MultiHopQA {
    fn new() -> Self {
        let decompose_sig = Signature::new("QuestionDecomposition", "Break down complex questions")
            .with_input(InputField::new("question", "Complex question", "String"))
            .with_output(OutputField::new(
                "sub_questions",
                "Simpler sub-questions",
                "String",
            ))
            .with_instructions("Break the question into 2-3 simpler sub-questions.");

        let answer_sig = Signature::new("MultiHopAnswer", "Answer based on sub-questions")
            .with_input(InputField::new("question", "Original question", "String"))
            .with_input(InputField::new("sub_questions", "Sub-questions", "String"))
            .with_output(OutputField::new("answer", "Final answer", "String"))
            .with_instructions("Answer the original question by considering the sub-questions.");

        Self {
            decomposer: ChainOfThought::new(decompose_sig),
            answerer: ChainOfThought::new(answer_sig),
            signature: Signature::new("MultiHopQA", "Multi-hop question answering"),
        }
    }
}

#[async_trait]
impl Module for MultiHopQA {
    fn signature(&self) -> &Signature {
        &self.signature
    }

    async fn forward(
        &self, inputs: HashMap<String, Value>,
    ) -> ModuleResult<HashMap<String, Value>> {
        // Decompose question
        let decomposition = self.decomposer.forward(inputs.clone()).await?;

        // Answer using decomposition
        let mut answer_inputs = inputs;
        if let Some(sub_q) = decomposition.get("sub_questions") {
            answer_inputs.insert("sub_questions".to_string(), sub_q.clone());
        }

        self.answerer.forward(answer_inputs).await
    }
}

#[tokio::main]
async fn main() -> Result<(), Box<dyn std::error::Error>> {
    tracing_subscriber::fmt()
        .with_max_level(tracing::Level::INFO)
        .init();

    println!("=== Advanced Pipeline Composition Example ===\n");

    // Part 1: Sequential Pipeline
    println!("--- Part 1: Sequential Pipeline (Extract → Summarize) ---\n");

    let sequential = SequentialPipeline::new();

    let text = "Apple Inc. announced today that CEO Tim Cook will speak at the \
                conference in San Francisco. Microsoft and Google representatives \
                will also attend.";

    let mut inputs = HashMap::new();
    inputs.insert("text".to_string(), json!(text));

    match sequential.forward(inputs).await {
        Ok(result) => {
            println!("Input: {}", text);
            println!(
                "Summary: {}\n",
                result.get("summary").unwrap_or(&json!("N/A"))
            );
        }
        Err(e) => {
            eprintln!("Error: {}\n", e);
            if std::env::var("GGEN_LLM_MODEL").is_err() {
                eprintln!("Note: Set GGEN_LLM_MODEL and appropriate API key");
                return Ok(());
            }
        }
    }

    // Part 2: Parallel Pipeline
    println!("--- Part 2: Parallel Pipeline (Multi-Perspective) ---\n");

    let parallel = ParallelPipeline::new();

    let analysis_text = "We're launching a new AI-powered search feature that uses \
                         machine learning to provide better results.";

    let mut inputs = HashMap::new();
    inputs.insert("text".to_string(), json!(analysis_text));

    match parallel.forward(inputs).await {
        Ok(result) => {
            println!("Input: {}", analysis_text);
            println!(
                "Analyses: {:#}",
                result.get("analyses").unwrap_or(&json!("N/A"))
            );
            println!();
        }
        Err(e) => {
            eprintln!("Error: {}\n", e);
        }
    }

    // Part 3: Conditional Pipeline
    println!("--- Part 3: Conditional Pipeline (Adaptive Routing) ---\n");

    let conditional = ConditionalPipeline::new();

    let questions = vec![
        "What is 2+2?",
        "Explain the philosophical implications of artificial consciousness \
         in the context of modern neuroscience.",
    ];

    for question in questions {
        let mut inputs = HashMap::new();
        inputs.insert("question".to_string(), json!(question));

        match conditional.forward(inputs).await {
            Ok(result) => {
                println!("Question: {}", question);
                if let Some(reasoning) = result.get("reasoning") {
                    println!("Reasoning: {}", reasoning);
                }
                println!(
                    "Answer: {}\n",
                    result.get("answer").unwrap_or(&json!("N/A"))
                );
            }
            Err(e) => {
                eprintln!("Error: {}\n", e);
            }
        }
    }

    // Part 4: Multi-Hop QA
    println!("--- Part 4: Multi-Hop QA (Question Decomposition) ---\n");

    let multihop = MultiHopQA::new();

    let complex_question = "Who was the president when the first iPhone was released, \
                           and what major economic event occurred during their presidency?";

    let mut inputs = HashMap::new();
    inputs.insert("question".to_string(), json!(complex_question));

    match multihop.forward(inputs).await {
        Ok(result) => {
            println!("Question: {}", complex_question);
            println!(
                "Answer: {}\n",
                result.get("answer").unwrap_or(&json!("N/A"))
            );
        }
        Err(e) => {
            eprintln!("Error: {}\n", e);
        }
    }

    println!("=== Example Complete ===");
    println!(
        "\nPipeline Patterns:\n\
         1. Sequential: Output of one module feeds next (Extract → Summarize)\n\
         2. Parallel: Multiple modules process same input concurrently\n\
         3. Conditional: Route based on classification or conditions\n\
         4. Multi-Hop: Break complex tasks into simpler sub-tasks\n\n\
         All patterns compose via the Module trait!"
    );

    Ok(())
}
