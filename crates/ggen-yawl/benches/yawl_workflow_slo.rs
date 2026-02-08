//! YAWL workflow generation SLO benchmarks.

use criterion::{black_box, criterion_group, criterion_main, Criterion, BenchmarkId};
use ggen_yawl::{OntologyLoader, YawlGenerator};

fn bench_rdf_loading(c: &mut Criterion) {
    let ontology = r#"
        @prefix ex: <http://example.org/> .
        @prefix owl: <http://www.w3.org/2002/07/owl#> .
        @prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .

        ex:TestProcess a owl:Class ;
            rdfs:label "Test Process" ;
            ex:stereo "BusinessProcess" .

        ex:TestStep a owl:Class ;
            rdfs:label "Test Step" ;
            rdfs:subClassOf ex:TestProcess .
    "#;

    c.bench_function("rdf_load_100_triples", |b| {
        b.iter(|| {
            let loader = OntologyLoader::new();
            black_box(loader.load_from_str(black_box(ontology), ggen_yawl::OntologyFormat::Turtle))
        })
    });
}

fn bench_workflow_generation(c: &mut Criterion) {
    let ontology = r#"
        @prefix ex: <http://example.org/> .
        @prefix owl: <http://www.w3.org/2002/07/owl#> .
        @prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .

        ex:AccountOpening a owl:Class ;
            rdfs:label "Account Opening" ;
            ex:stereo "BusinessProcess" .

        ex:ValidateIdentity a owl:Class ;
            rdfs:label "Validate Identity" ;
            rdfs:subClassOf ex:AccountOpening ;
            ex:stereo "ProcessStep" .

        ex:CreditCheck a owl:Class ;
            rdfs:label "Credit Check" ;
            rdfs:subClassOf ex:AccountOpening ;
            ex:stereo "ProcessStep" .

        ex:hasNextStep a owl:ObjectProperty ;
            rdfs:domain ex:ValidateIdentity ;
            rdfs:range ex:CreditCheck .
    "#;

    c.bench_function("workflow_generation_simple", |b| {
        b.iter(|| {
            let generator = YawlGenerator::new();
            black_box(generator.generate(black_box(ontology)))
        })
    });
}

fn bench_template_rendering(c: &mut Criterion) {
    let template_context = ggen_yawl::template::TemplateContext {
        workflow_name: "TestWorkflow".to_string(),
        description: "Test workflow for benchmarking".to_string(),
        version: "1.0.0".to_string(),
        tasks: vec![
            ggen_yawl::template::TaskContext {
                id: "t1".to_string(),
                name: "Task 1".to_string(),
                split_type: "AND".to_string(),
                join_type: "XOR".to_string(),
                is_auto: true,
                decomposition_id: None,
            },
        ],
        flows: vec![],
        input_condition: None,
        output_condition: None,
        variables: vec![],
    };

    c.bench_function("template_render_single_task", |b| {
        b.iter(|| {
            let renderer = ggen_yawl::template::TemplateRenderer::new();
            black_box(renderer.render_yawl_xml(black_box(&template_context)))
        })
    });
}

criterion_group!(
    benches,
    bench_rdf_loading,
    bench_workflow_generation,
    bench_template_rendering
);
criterion_main!(benches);
