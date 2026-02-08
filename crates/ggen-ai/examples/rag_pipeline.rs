//! Example: RAG Pipeline with Multi-Hop Reasoning
//!
//! Demonstrates a Retrieval-Augmented Generation pattern:
//! 1. Question decomposition
//! 2. Context retrieval (simulated)
//! 3. Multi-hop reasoning
//! 4. Answer synthesis
//!
//! Note: This example simulates retrieval. For production RAG,
//! integrate with vector databases (Qdrant, Milvus, etc.)
//!
//! Run with: cargo run --example rag_pipeline
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

/// Simulated document store for RAG
struct DocumentStore {
    documents: HashMap<String, Vec<String>>,
}

impl DocumentStore {
    fn new() -> Self {
        let mut documents = HashMap::new();

        // Technology documents
        documents.insert(
            "rust".to_string(),
            vec![
                "Rust is a systems programming language focused on safety, speed, and concurrency.".to_string(),
                "Rust uses ownership and borrowing to guarantee memory safety without garbage collection.".to_string(),
                "The Rust compiler enforces memory safety at compile time through its borrow checker.".to_string(),
            ],
        );

        documents.insert(
            "ai".to_string(),
            vec![
                "Artificial Intelligence (AI) enables machines to perform tasks requiring human intelligence.".to_string(),
                "Large Language Models (LLMs) like GPT-4 are trained on vast amounts of text data.".to_string(),
                "DSPy is a framework for programming LLM applications declaratively.".to_string(),
            ],
        );

        documents.insert(
            "databases".to_string(),
            vec![
                "Vector databases like Qdrant store high-dimensional embeddings for similarity search.".to_string(),
                "Traditional databases like PostgreSQL use SQL for structured data queries.".to_string(),
                "NoSQL databases like MongoDB provide flexible document-based storage.".to_string(),
            ],
        );

        Self { documents }
    }

    /// Simulate retrieval by keyword matching
    fn retrieve(&self, query: &str, top_k: usize) -> Vec<String> {
        let query_lower = query.to_lowercase();
        let mut results = Vec::new();

        for docs in self.documents.values() {
            for doc in docs {
                let doc_lower = doc.to_lowercase();
                // Simple keyword matching
                if query_lower
                    .split_whitespace()
                    .any(|word| doc_lower.contains(word))
                {
                    results.push(doc.clone());
                }
            }
        }

        results.truncate(top_k);
        results
    }
}

/// RAG Pipeline Module
struct RAGPipeline {
    decomposer: ChainOfThought,
    retriever: DocumentStore,
    synthesizer: ChainOfThought,
    signature: Signature,
}

impl RAGPipeline {
    fn new() -> Self {
        // Step 1: Decompose complex questions
        let decompose_sig = Signature::new(
            "QuestionDecomposition",
            "Break complex questions into search queries",
        )
        .with_input(InputField::new("question", "Complex question", "String"))
        .with_output(OutputField::new(
            "queries",
            "Search queries (comma-separated)",
            "String",
        ))
        .with_instructions(
            "Break down the question into 2-3 simple search queries. \
             Each query should target a specific piece of information needed.",
        );

        // Step 2: Synthesize answer from context
        let synthesize_sig =
            Signature::new("AnswerSynthesis", "Generate answer from retrieved context")
                .with_input(InputField::new("question", "Original question", "String"))
                .with_input(InputField::new("context", "Retrieved documents", "String"))
                .with_output(OutputField::new("answer", "Comprehensive answer", "String"))
                .with_output(OutputField::new(
                    "citations",
                    "Citation indices used",
                    "String",
                ))
                .with_instructions(
                    "Answer the question using ONLY information from the provided context. \
             Include citation numbers [1], [2], etc. for each fact used. \
             If the context doesn't contain enough information, say so.",
                );

        Self {
            decomposer: ChainOfThought::new(decompose_sig),
            retriever: DocumentStore::new(),
            synthesizer: ChainOfThought::new(synthesize_sig),
            signature: Signature::new("RAGPipeline", "Retrieval-augmented QA"),
        }
    }
}

#[async_trait]
impl Module for RAGPipeline {
    fn signature(&self) -> &Signature {
        &self.signature
    }

    async fn forward(
        &self, inputs: HashMap<String, Value>,
    ) -> ModuleResult<HashMap<String, Value>> {
        // Extract question
        let question = inputs
            .get("question")
            .and_then(|v| v.as_str())
            .ok_or_else(|| ModuleError::Other("Missing question".to_string()))?;

        println!("\n[RAG Pipeline]");
        println!("Question: {}", question);

        // Step 1: Decompose question into queries
        println!("\n[Step 1: Decomposition]");
        let decomp_result = self.decomposer.forward(inputs.clone()).await?;

        let queries = decomp_result
            .get("queries")
            .and_then(|v| v.as_str())
            .unwrap_or("");

        println!("Search Queries: {}", queries);

        // Step 2: Retrieve documents for each query
        println!("\n[Step 2: Retrieval]");
        let mut all_docs = Vec::new();

        for query in queries.split(',') {
            let query = query.trim();
            if !query.is_empty() {
                println!("  Searching: {}", query);
                let docs = self.retriever.retrieve(query, 2);
                all_docs.extend(docs);
            }
        }

        // Remove duplicates
        all_docs.sort();
        all_docs.dedup();

        println!("  Retrieved {} documents", all_docs.len());

        // Step 3: Format context with citations
        let context = all_docs
            .iter()
            .enumerate()
            .map(|(i, doc)| format!("[{}] {}", i + 1, doc))
            .collect::<Vec<_>>()
            .join("\n\n");

        println!("\n[Step 3: Synthesis]");
        println!("Context:\n{}\n", context);

        // Step 4: Synthesize answer
        let mut synth_inputs = HashMap::new();
        synth_inputs.insert("question".to_string(), json!(question));
        synth_inputs.insert("context".to_string(), json!(context));

        self.synthesizer.forward(synth_inputs).await
    }
}

/// Enhanced RAG with Query Rewriting
struct EnhancedRAGPipeline {
    query_rewriter: Predictor,
    base_rag: RAGPipeline,
    signature: Signature,
}

impl EnhancedRAGPipeline {
    fn new() -> Self {
        let rewrite_sig = Signature::new(
            "QueryRewriting",
            "Optimize search queries for better retrieval",
        )
        .with_input(InputField::new(
            "original_query",
            "Original query",
            "String",
        ))
        .with_output(OutputField::new(
            "optimized_query",
            "Improved search query",
            "String",
        ))
        .with_instructions(
            "Rewrite the query to be more specific and likely to retrieve relevant documents. \
             Use technical terms and key concepts.",
        );

        Self {
            query_rewriter: Predictor::new(rewrite_sig),
            base_rag: RAGPipeline::new(),
            signature: Signature::new("EnhancedRAG", "RAG with query optimization"),
        }
    }
}

#[async_trait]
impl Module for EnhancedRAGPipeline {
    fn signature(&self) -> &Signature {
        &self.signature
    }

    async fn forward(
        &self, inputs: HashMap<String, Value>,
    ) -> ModuleResult<HashMap<String, Value>> {
        // Rewrite query first
        let original_query = inputs
            .get("question")
            .and_then(|v| v.as_str())
            .unwrap_or("");

        println!("\n[Enhanced RAG with Query Rewriting]");
        println!("Original Query: {}", original_query);

        let rewrite_input = HashMap::from([("original_query".to_string(), json!(original_query))]);

        let rewrite_result = self.query_rewriter.forward(rewrite_input).await?;

        let optimized = rewrite_result
            .get("optimized_query")
            .and_then(|v| v.as_str())
            .unwrap_or(original_query);

        println!("Optimized Query: {}", optimized);

        // Use optimized query for RAG
        let mut rag_inputs = inputs;
        rag_inputs.insert("question".to_string(), json!(optimized));

        self.base_rag.forward(rag_inputs).await
    }
}

#[tokio::main]
async fn main() -> Result<(), Box<dyn std::error::Error>> {
    tracing_subscriber::fmt()
        .with_max_level(tracing::Level::INFO)
        .init();

    println!("=== RAG Pipeline Example ===\n");

    // Part 1: Basic RAG Pipeline
    println!("--- Part 1: Basic RAG Pipeline ---");

    let rag = RAGPipeline::new();

    let questions = vec![
        "What is Rust and how does it ensure memory safety?",
        "Explain how vector databases work and their use in AI applications.",
    ];

    for question in &questions {
        let mut inputs = HashMap::new();
        inputs.insert("question".to_string(), json!(question));

        match rag.forward(inputs).await {
            Ok(result) => {
                println!("\n[Result]");
                println!(
                    "Answer: {}",
                    result
                        .get("answer")
                        .and_then(|v| v.as_str())
                        .unwrap_or("N/A")
                );
                println!(
                    "Citations: {}",
                    result
                        .get("citations")
                        .and_then(|v| v.as_str())
                        .unwrap_or("N/A")
                );
                println!("\n{}", "=".repeat(60));
            }
            Err(e) => {
                eprintln!("Error: {}", e);
                if std::env::var("GGEN_LLM_MODEL").is_err() {
                    eprintln!("\nNote: Set GGEN_LLM_MODEL and appropriate API key");
                    return Ok(());
                }
            }
        }
    }

    // Part 2: Enhanced RAG with Query Rewriting
    println!("\n--- Part 2: Enhanced RAG with Query Rewriting ---");

    let enhanced_rag = EnhancedRAGPipeline::new();

    let complex_question = "How do modern programming languages like Rust handle AI development?";

    let mut inputs = HashMap::new();
    inputs.insert("question".to_string(), json!(complex_question));

    match enhanced_rag.forward(inputs).await {
        Ok(result) => {
            println!("\n[Result]");
            println!(
                "Answer: {}",
                result
                    .get("answer")
                    .and_then(|v| v.as_str())
                    .unwrap_or("N/A")
            );
            println!(
                "Citations: {}",
                result
                    .get("citations")
                    .and_then(|v| v.as_str())
                    .unwrap_or("N/A")
            );
        }
        Err(e) => {
            eprintln!("Error: {}", e);
        }
    }

    println!("\n=== Example Complete ===");
    println!(
        "\nRAG Pipeline Components:\n\
         1. Question Decomposition - Break complex questions into sub-queries\n\
         2. Document Retrieval - Fetch relevant documents (simulated here)\n\
         3. Answer Synthesis - Generate answer from retrieved context\n\
         4. Citation Tracking - Track which documents were used\n\n\
         Production Enhancements:\n\
         - Replace DocumentStore with vector database (Qdrant, Pinecone)\n\
         - Add embedding generation for semantic search\n\
         - Implement re-ranking for better relevance\n\
         - Add caching for frequently accessed documents\n\
         - Use assertions to validate answer quality"
    );

    Ok(())
}
