use data_pipeline_cli::{Pipeline, Source, Transform, Sink};
use tempfile::TempDir;

#[tokio::test]
async fn test_csv_to_rdf_pipeline() {
    let temp_dir = TempDir::new().unwrap();
    let output_path = temp_dir.path().join("output.db");

    // Create test CSV
    let csv_path = temp_dir.path().join("test.csv");
    std::fs::write(&csv_path, "name,age\nAlice,30\nBob,25").unwrap();

    // Build pipeline
    let pipeline = Pipeline::builder()
        .name("test-pipeline")
        .source(Source::csv(csv_path.to_str().unwrap()).unwrap())
        .transform(Transform::map(vec![
            ("name", "http://xmlns.com/foaf/0.1/name"),
            ("age", "http://xmlns.com/foaf/0.1/age"),
        ]).unwrap())
        .sink(Sink::rdf(&format!("oxigraph://{}", output_path.display())).unwrap())
        .build()
        .unwrap();

    // Run pipeline
    let result = pipeline.run().await.unwrap();

    assert_eq!(result.total_records, 2);
    assert_eq!(result.successful_records, 2);
}

#[tokio::test]
async fn test_filtering_transform() {
    let temp_dir = TempDir::new().unwrap();
    let csv_path = temp_dir.path().join("test.csv");

    std::fs::write(&csv_path, "name,age\nAlice,30\nBob,17\nCarol,25").unwrap();

    let pipeline = Pipeline::builder()
        .name("filter-test")
        .source(Source::csv(csv_path.to_str().unwrap()).unwrap())
        .transform(Transform::filter("age >= 18").unwrap())
        .sink(Sink::json(temp_dir.path().join("output.json").to_str().unwrap()).unwrap())
        .build()
        .unwrap();

    let result = pipeline.run().await.unwrap();

    assert_eq!(result.total_records, 3);
    assert_eq!(result.successful_records, 2); // Only Alice and Carol
    assert_eq!(result.filtered_records, 1); // Bob filtered out
}

#[test]
fn test_pipeline_builder() {
    let pipeline = Pipeline::builder()
        .name("builder-test")
        .description("Test pipeline builder")
        .batch_size(1000)
        .parallelism(4)
        .build();

    assert!(pipeline.is_ok());
    let pipeline = pipeline.unwrap();
    assert_eq!(pipeline.name(), "builder-test");
}
