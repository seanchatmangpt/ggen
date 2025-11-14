// Document Management System - Rust Example
// Demonstrates document operations, versioning, workflow, and search

use oxigraph::store::Store;
use oxigraph::sparql::QueryResults;
use oxigraph::model::*;
use std::error::Error;

const DMS_NS: &str = "https://ggen.ai/ontology/document-management#";

#[tokio::main]
async fn main() -> Result<(), Box<dyn Error>> {
    println!("Document Management System - Rust Example\n");

    let store = Store::new()?;

    // Example 1: Create document with metadata
    create_document(&store)?;

    // Example 2: Version management
    create_version(&store)?;

    // Example 3: Workflow and approval
    create_workflow(&store)?;

    // Example 4: Search and OCR
    search_documents(&store)?;

    println!("\nAll operations completed successfully!");
    Ok(())
}

fn create_document(store: &Store) -> Result<(), Box<dyn Error>> {
    println!("1. Creating document with metadata...");

    let doc_uri = NamedNode::new(format!("{}doc001", DMS_NS))?;
    let title_pred = NamedNode::new(format!("{}hasTitle", DMS_NS))?;
    let filename_pred = NamedNode::new(format!("{}hasFileName", DMS_NS))?;
    let mime_pred = NamedNode::new(format!("{}hasMimeType", DMS_NS))?;

    store.insert(&Quad::new(
        doc_uri.clone(),
        NamedNode::new("http://www.w3.org/1999/02/22-rdf-syntax-ns#type")?,
        NamedNode::new(format!("{}Document", DMS_NS))?,
        GraphName::DefaultGraph,
    ))?;

    store.insert(&Quad::new(
        doc_uri.clone(),
        title_pred,
        Literal::new_simple_literal("Project Proposal 2025"),
        GraphName::DefaultGraph,
    ))?;

    store.insert(&Quad::new(
        doc_uri.clone(),
        filename_pred,
        Literal::new_simple_literal("proposal.docx"),
        GraphName::DefaultGraph,
    ))?;

    store.insert(&Quad::new(
        doc_uri,
        mime_pred,
        Literal::new_simple_literal("application/vnd.openxmlformats-officedocument.wordprocessingml.document"),
        GraphName::DefaultGraph,
    ))?;

    println!("   ✓ Document created with metadata");
    Ok(())
}

fn create_version(store: &Store) -> Result<(), Box<dyn Error>> {
    println!("2. Creating document version...");

    let doc_uri = NamedNode::new(format!("{}doc001", DMS_NS))?;
    let version_uri = NamedNode::new(format!("{}version001", DMS_NS))?;
    let has_version_pred = NamedNode::new(format!("{}hasVersion", DMS_NS))?;
    let version_num_pred = NamedNode::new(format!("{}versionNumber", DMS_NS))?;
    let is_current_pred = NamedNode::new(format!("{}isCurrentVersion", DMS_NS))?;

    store.insert(&Quad::new(
        doc_uri,
        has_version_pred,
        version_uri.clone(),
        GraphName::DefaultGraph,
    ))?;

    store.insert(&Quad::new(
        version_uri.clone(),
        version_num_pred,
        Literal::new_typed_literal("1.0", NamedNode::new("http://www.w3.org/2001/XMLSchema#decimal")?),
        GraphName::DefaultGraph,
    ))?;

    store.insert(&Quad::new(
        version_uri,
        is_current_pred,
        Literal::new_typed_literal("true", NamedNode::new("http://www.w3.org/2001/XMLSchema#boolean")?),
        GraphName::DefaultGraph,
    ))?;

    println!("   ✓ Version 1.0 created and marked as current");
    Ok(())
}

fn create_workflow(store: &Store) -> Result<(), Box<dyn Error>> {
    println!("3. Creating approval workflow...");

    let doc_uri = NamedNode::new(format!("{}doc001", DMS_NS))?;
    let workflow_uri = NamedNode::new(format!("{}workflow001", DMS_NS))?;
    let has_workflow_pred = NamedNode::new(format!("{}hasWorkflow", DMS_NS))?;
    let status_pred = NamedNode::new(format!("{}workflowStatus", DMS_NS))?;

    store.insert(&Quad::new(
        doc_uri,
        has_workflow_pred,
        workflow_uri.clone(),
        GraphName::DefaultGraph,
    ))?;

    store.insert(&Quad::new(
        workflow_uri,
        status_pred,
        Literal::new_simple_literal("pending"),
        GraphName::DefaultGraph,
    ))?;

    println!("   ✓ Approval workflow created with pending status");
    Ok(())
}

fn search_documents(store: &Store) -> Result<(), Box<dyn Error>> {
    println!("4. Searching documents...");

    let query = format!(r#"
        PREFIX dms: <{}>
        SELECT ?doc ?title ?filename
        WHERE {{
            ?doc a dms:Document ;
                 dms:hasTitle ?title ;
                 dms:hasFileName ?filename .
        }}
    "#, DMS_NS);

    if let QueryResults::Solutions(solutions) = store.query(&query)? {
        for solution in solutions {
            let solution = solution?;
            println!("   Found document:");
            if let Some(title) = solution.get("title") {
                println!("     Title: {}", title);
            }
            if let Some(filename) = solution.get("filename") {
                println!("     Filename: {}", filename);
            }
        }
    }

    Ok(())
}
