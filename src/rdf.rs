use oxigraph::model::*;
use oxigraph::store::Store;
use std::path::PathBuf;
use anyhow::Result;
use crate::models::{FileEntry, DetectedCapability, Receipt};

pub fn project_to_rdf(files: &[FileEntry], capabilities: &[DetectedCapability], receipt: &Receipt, out: &PathBuf) -> Result<()> {
    let store = Store::new()?;
    
    // Namespaces
    let doap = "http://usefulinc.com/ns/doap#";
    let prov = "http://www.w3.org/ns/prov#";
    let spdx = "http://spdx.org/rdf/terms#";
    let skos = "http://www.w3.org/2004/02/skos/core#";
    let rdf_type = NamedNode::new("http://www.w3.org/1999/02/22-rdf-syntax-ns#type")?;
    let dcat = "http://www.w3.org/ns/dcat#";
    let dcterms = "http://purl.org/dc/terms/";
    
    // Catalog
    let catalog_id = format!("urn:cpmp:catalog:{}", receipt.id);
    let catalog_node = NamedNode::new(catalog_id.clone())?;
    store.insert(Quad::new(catalog_node.clone(), rdf_type.clone(), NamedNode::new(format!("{}Catalog", dcat))?, GraphName::DefaultGraph))?;

    // Current Scan Activity
    let scan_id = format!("urn:cpmp:scan:{}", receipt.id);
    let scan_node = NamedNode::new(scan_id.clone())?;
    store.insert(Quad::new(
        scan_node.clone(),
        rdf_type.clone(),
        NamedNode::new(format!("{}Activity", prov))?,
        GraphName::DefaultGraph,
    ))?;
    
    store.insert(Quad::new(
        scan_node.clone(),
        NamedNode::new(format!("{}startedAtTime", prov))?,
        Literal::new_typed_literal(&receipt.timestamp, NamedNode::new("http://www.w3.org/2001/XMLSchema#dateTime")?),
        GraphName::DefaultGraph,
    ))?;

    // Project Root
    let project_node = NamedNode::new("urn:cpmp:project:local")?;
    store.insert(Quad::new(
        project_node.clone(),
        rdf_type.clone(),
        NamedNode::new(format!("{}Project", doap))?,
        GraphName::DefaultGraph,
    ))?;
    store.insert(Quad::new(catalog_node.clone(), NamedNode::new(format!("{}dataset", dcat))?, project_node.clone(), GraphName::DefaultGraph))?;

    // Files
    for file in files {
        let file_uri = format!("file://{}", file.path);
        let file_node = NamedNode::new(file_uri.clone())?;
        
        store.insert(Quad::new(
            file_node.clone(),
            rdf_type.clone(),
            NamedNode::new(format!("{}File", spdx))?,
            GraphName::DefaultGraph,
        ))?;
        
        store.insert(Quad::new(
            file_node.clone(),
            NamedNode::new(format!("{}wasGeneratedBy", prov))?,
            scan_node.clone(),
            GraphName::DefaultGraph,
        ))?;
        
        // checksum
        let checksum_node = NamedNode::new(format!("urn:cpmp:checksum:{}", file.hash))?;
        store.insert(Quad::new(checksum_node.clone(), rdf_type.clone(), NamedNode::new(format!("{}Checksum", spdx))?, GraphName::DefaultGraph))?;
        store.insert(Quad::new(checksum_node.clone(), NamedNode::new(format!("{}algorithm", spdx))?, NamedNode::new("http://spdx.org/rdf/terms#checksumAlgorithm_blake3")?, GraphName::DefaultGraph))?;
        store.insert(Quad::new(checksum_node.clone(), NamedNode::new(format!("{}checksumValue", spdx))?, Literal::new_simple_literal(&file.hash), GraphName::DefaultGraph))?;
        store.insert(Quad::new(file_node.clone(), NamedNode::new(format!("{}checksum", spdx))?, checksum_node.clone(), GraphName::DefaultGraph))?;
    }
    
    // Capabilities
    for cap in capabilities {
        let file_uri = format!("file://{}", cap.file_path);
        let file_node = NamedNode::new(file_uri)?;
        
        let cap_uri = format!("urn:cpmp:capability:{}", cap.capability.replace(" ", "_"));
        let cap_node = NamedNode::new(cap_uri)?;
        
        store.insert(Quad::new(
            cap_node.clone(),
            rdf_type.clone(),
            NamedNode::new(format!("{}Concept", skos))?,
            GraphName::DefaultGraph,
        ))?;
        
        store.insert(Quad::new(
            cap_node.clone(),
            NamedNode::new(format!("{}prefLabel", skos))?,
            Literal::new_simple_literal(&cap.capability),
            GraphName::DefaultGraph,
        ))?;

        store.insert(Quad::new(
            file_node,
            NamedNode::new("urn:cpmp:implementsCapability")?,
            cap_node,
            GraphName::DefaultGraph,
        ))?;
    }
    
    std::fs::create_dir_all(out.join("catalog"))?;
    
    // Dump to N-Quads
    let nq_path = out.join("catalog/cpmp-catalog.nq");
    let mut file_nq = std::fs::File::create(&nq_path)?;
    store.dump_dataset(oxigraph::io::DatasetFormat::NQuads, &mut file_nq)?;
    
    // Dump to Turtle
    let ttl_path = out.join("catalog/cpmp-catalog.ttl");
    let mut file_ttl = std::fs::File::create(&ttl_path)?;
    store.dump_graph(&GraphName::DefaultGraph, oxigraph::io::GraphFormat::Turtle, &mut file_ttl)?;
    
    // Emit Shapes
    let shapes_ttl = r#"
@prefix sh: <http://www.w3.org/ns/shacl#> .
@prefix doap: <http://usefulinc.com/ns/doap#> .
@prefix spdx: <http://spdx.org/rdf/terms#> .
@prefix prov: <http://www.w3.org/ns/prov#> .
@prefix xsd: <http://www.w3.org/2001/XMLSchema#> .
@prefix skos: <http://www.w3.org/2004/02/skos/core#> .

<#FileShape> a sh:NodeShape ;
    sh:targetClass spdx:File ;
    sh:property [
        sh:path prov:wasGeneratedBy ;
        sh:minCount 1 ;
        sh:class prov:Activity ;
    ] .

<#CapabilityShape> a sh:NodeShape ;
    sh:targetClass skos:Concept ;
    sh:property [
        sh:path skos:prefLabel ;
        sh:minCount 1 ;
        sh:datatype xsd:string ;
    ] .
"#;
    let shapes_path = out.join("catalog/cpmp-shapes.ttl");
    std::fs::write(&shapes_path, shapes_ttl)?;

    Ok(())
}
