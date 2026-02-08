//! Factory Functions for Common Test Data
//!
//! Provides convenient factory functions for creating common test patterns.

use crate::dspy::field::{InputField, OutputField};
use crate::dspy::optimizer::Example;
use crate::dspy::signature::Signature;
use crate::dspy::testing::ExampleBuilder;

/// Create a QA (Question-Answer) example
///
/// # Example
///
/// ```
/// use ggen_ai::dspy::testing::qa_example;
///
/// let example = qa_example("What is Rust?", "A programming language");
/// ```
pub fn qa_example(question: &str, answer: &str) -> Example {
    ExampleBuilder::new()
        .input("question", question)
        .output("answer", answer)
        .build()
}

/// Create a classification example
///
/// # Example
///
/// ```
/// use ggen_ai::dspy::testing::classification_example;
///
/// let example = classification_example("Great product!", "positive");
/// ```
pub fn classification_example(text: &str, label: &str) -> Example {
    ExampleBuilder::new()
        .input("text", text)
        .output("label", label)
        .build()
}

/// Create a summarization example
pub fn summarization_example(document: &str, summary: &str) -> Example {
    ExampleBuilder::new()
        .input("document", document)
        .output("summary", summary)
        .build()
}

/// Create a translation example
pub fn translation_example(
    source: &str, target: &str, source_lang: &str, target_lang: &str,
) -> Example {
    ExampleBuilder::new()
        .input("source_text", source)
        .input("source_language", source_lang)
        .input("target_language", target_lang)
        .output("translated_text", target)
        .build()
}

/// Create a standard QA training set
///
/// # Example
///
/// ```
/// use ggen_ai::dspy::testing::create_qa_trainset;
///
/// let trainset = create_qa_trainset();
/// assert!(trainset.len() >= 5);
/// ```
pub fn create_qa_trainset() -> Vec<Example> {
    vec![
        qa_example("What is Rust?", "A systems programming language"),
        qa_example(
            "What is DSPy?",
            "A framework for programming language models",
        ),
        qa_example("What is ggen?", "An ontology-driven code generator"),
        qa_example("What is 2+2?", "4"),
        qa_example("What is the capital of France?", "Paris"),
    ]
}

/// Create a classification training set
pub fn create_classification_trainset() -> Vec<Example> {
    vec![
        classification_example("Great product, highly recommend!", "positive"),
        classification_example("Terrible quality, waste of money", "negative"),
        classification_example("It's okay, nothing special", "neutral"),
        classification_example("Absolutely love it!", "positive"),
        classification_example("Disappointed with purchase", "negative"),
    ]
}

/// Create a QA signature for testing
///
/// # Example
///
/// ```
/// use ggen_ai::dspy::testing::create_test_signature;
///
/// let sig = create_test_signature("QA", "Answer questions");
/// assert_eq!(sig.name, "QA");
/// ```
pub fn create_test_signature(name: &str, description: &str) -> Signature {
    Signature::new(name, description)
        .with_input(InputField::new(
            "question",
            "The question to answer",
            "String",
        ))
        .with_output(OutputField::new(
            "answer",
            "The answer to the question",
            "String",
        ))
}

/// Create a classification signature for testing
pub fn create_classification_signature() -> Signature {
    Signature::new("Classify", "Classify text sentiment")
        .with_input(InputField::new("text", "Text to classify", "String"))
        .with_output(OutputField::new("label", "Classification label", "String"))
}

/// Create a chain-of-thought signature for testing
pub fn create_cot_signature() -> Signature {
    Signature::new("ChainOfThought", "Reasoning with intermediate steps")
        .with_input(InputField::new("question", "Question to answer", "String"))
        .with_output(OutputField::new(
            "reasoning",
            "Step-by-step reasoning",
            "String",
        ))
        .with_output(OutputField::new("answer", "Final answer", "String"))
}

/// Create a large training set for performance testing
pub fn create_large_trainset(size: usize) -> Vec<Example> {
    (0..size)
        .map(|i| qa_example(&format!("Question {}", i), &format!("Answer {}", i)))
        .collect()
}

/// Create examples with varying complexity
pub fn create_complexity_trainset() -> Vec<Example> {
    vec![
        // Simple
        qa_example("Hi", "Hello"),
        // Medium
        qa_example(
            "What is Rust?",
            "Rust is a systems programming language focused on safety and performance",
        ),
        // Complex
        qa_example(
            "Explain the difference between ownership and borrowing in Rust",
            "Ownership is Rust's core concept where each value has a single owner. \
             Borrowing allows temporary access to owned data through references, \
             enforcing memory safety at compile time without garbage collection.",
        ),
    ]
}

/// Create examples for edge case testing
pub fn create_edge_case_examples() -> Vec<Example> {
    vec![
        // Empty
        qa_example("", ""),
        // Very long
        qa_example(&"What is ".repeat(100), &"Answer ".repeat(100)),
        // Unicode
        qa_example("你好吗？", "我很好，谢谢"),
        // Special characters
        qa_example("What is @#$%?", "Special characters"),
        // Numbers only
        qa_example("123456", "789012"),
    ]
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_qa_example() {
        let example = qa_example("Question", "Answer");
        assert_eq!(example.inputs.len(), 1);
        assert_eq!(example.outputs.len(), 1);
        assert!(example.inputs.contains_key("question"));
        assert!(example.outputs.contains_key("answer"));
    }

    #[test]
    fn test_classification_example() {
        let example = classification_example("Text", "Label");
        assert_eq!(example.inputs.len(), 1);
        assert_eq!(example.outputs.len(), 1);
        assert!(example.inputs.contains_key("text"));
        assert!(example.outputs.contains_key("label"));
    }

    #[test]
    fn test_summarization_example() {
        let example = summarization_example("Document", "Summary");
        assert!(example.inputs.contains_key("document"));
        assert!(example.outputs.contains_key("summary"));
    }

    #[test]
    fn test_translation_example() {
        let example = translation_example("Hello", "Bonjour", "English", "French");
        assert_eq!(example.inputs.len(), 3);
        assert_eq!(example.outputs.len(), 1);
    }

    #[test]
    fn test_create_qa_trainset() {
        let trainset = create_qa_trainset();
        assert!(trainset.len() >= 5);
        assert!(trainset
            .iter()
            .all(|ex| !ex.inputs.is_empty() && !ex.outputs.is_empty()));
    }

    #[test]
    fn test_create_classification_trainset() {
        let trainset = create_classification_trainset();
        assert!(trainset.len() >= 5);
    }

    #[test]
    fn test_create_test_signature() {
        let sig = create_test_signature("Test", "Description");
        assert_eq!(sig.name, "Test");
        assert_eq!(sig.description, "Description");
        assert_eq!(sig.inputs.len(), 1);
        assert_eq!(sig.outputs.len(), 1);
    }

    #[test]
    fn test_create_classification_signature() {
        let sig = create_classification_signature();
        assert_eq!(sig.inputs.len(), 1);
        assert_eq!(sig.outputs.len(), 1);
        assert_eq!(sig.inputs[0].name(), "text");
        assert_eq!(sig.outputs[0].name(), "label");
    }

    #[test]
    fn test_create_cot_signature() {
        let sig = create_cot_signature();
        assert_eq!(sig.inputs.len(), 1);
        assert_eq!(sig.outputs.len(), 2);
        assert!(sig.outputs.iter().any(|f| f.name() == "reasoning"));
        assert!(sig.outputs.iter().any(|f| f.name() == "answer"));
    }

    #[test]
    fn test_create_large_trainset() {
        let trainset = create_large_trainset(100);
        assert_eq!(trainset.len(), 100);
    }

    #[test]
    fn test_create_complexity_trainset() {
        let trainset = create_complexity_trainset();
        assert_eq!(trainset.len(), 3);

        // Verify varying sizes
        let lengths: Vec<usize> = trainset
            .iter()
            .map(|ex| {
                ex.inputs
                    .values()
                    .next()
                    .and_then(|v| v.as_str())
                    .map(|s| s.len())
                    .unwrap_or(0)
            })
            .collect();

        assert!(lengths[0] < lengths[1]);
        assert!(lengths[1] < lengths[2]);
    }

    #[test]
    fn test_create_edge_case_examples() {
        let examples = create_edge_case_examples();
        assert!(examples.len() >= 5);

        // Check that we have various edge cases
        let has_empty = examples.iter().any(|ex| {
            ex.inputs
                .values()
                .next()
                .and_then(|v| v.as_str())
                .map(|s| s.is_empty())
                .unwrap_or(false)
        });
        assert!(has_empty);
    }
}
