use ai_code_generation::*;
use uuid::Uuid;

#[tokio::main]
async fn main() -> anyhow::Result<()> {
    // Create mock LLM
    let llm = Box::new(MockLlm::new());
    let generator = CodeGenerator::new(llm);

    println!("=== AI Code Generation Demo ===\n");

    // Generate Rust code
    println!("1. Generating Rust code (Simple)...");
    let rust_req = CodeGenRequest {
        id: Uuid::new_v4(),
        prompt: "Generate a Rust function".to_string(),
        language: ProgrammingLanguage::Rust,
        complexity: Complexity::Simple,
    };

    let rust_code = generator.generate(rust_req).await?;
    println!("Generated {} lines of Rust code", rust_code.metrics.lines_of_code);
    println!("Functions: {}, Complexity: {}\n", rust_code.metrics.functions, rust_code.metrics.complexity_score);

    // Generate Python code
    println!("2. Generating Python code (Intermediate)...");
    let python_req = CodeGenRequest {
        id: Uuid::new_v4(),
        prompt: "Generate a Python function".to_string(),
        language: ProgrammingLanguage::Python,
        complexity: Complexity::Intermediate,
    };

    let python_code = generator.generate(python_req).await?;
    println!("Generated {} lines of Python code", python_code.metrics.lines_of_code);
    println!("Functions: {}, Complexity: {}\n", python_code.metrics.functions, python_code.metrics.complexity_score);

    // Generate TypeScript code
    println!("3. Generating TypeScript code (Advanced)...");
    let ts_req = CodeGenRequest {
        id: Uuid::new_v4(),
        prompt: "Generate a TypeScript function".to_string(),
        language: ProgrammingLanguage::TypeScript,
        complexity: Complexity::Advanced,
    };

    let ts_code = generator.generate(ts_req).await?;
    println!("Generated {} lines of TypeScript code", ts_code.metrics.lines_of_code);
    println!("Functions: {}, Complexity: {}\n", ts_code.metrics.functions, ts_code.metrics.complexity_score);

    // Refine generated code
    println!("4. Refining generated code...");
    let refined = generator.refine(&rust_code.code, "add error handling").await?;
    println!("Refined code length: {} chars\n", refined.len());

    println!("✅ Demo complete!");
    Ok(())
}
