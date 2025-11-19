//! Integration tests for ggen-xeno

use ggen_xeno::{
    archaeology::{Artifact, ArchaeologyMission, MissionStatus, analyzer::StatisticalAnalyzer},
    ontology::{Concept, ConceptType, Relation, RelationType, ValidationReport, XenoOntology, CognitiveArchitecture},
    systems::{CrystallineOntology, QuantumOntology, CollectiveOntology, TemporalOntology},
    paradigms::ComputationalParadigm,
};

#[tokio::test]
async fn test_crystalline_ontology_basic() {
    let mut ontology = CrystallineOntology::new("Test-Civilization");

    assert_eq!(ontology.architecture(), CognitiveArchitecture::Crystalline);

    let concept = ontology.create_pattern_concept(
        "test-pattern".to_string(),
        vec![440.0, 550.0, 660.0],
    );

    ontology.add_concept(concept.clone()).await.unwrap();

    let concepts = ontology.query_concepts("*").await.unwrap();
    assert_eq!(concepts.len(), 1);
    assert_eq!(concepts[0].id, concept.id);
}

#[tokio::test]
async fn test_quantum_ontology_superposition() {
    let mut ontology = QuantumOntology::new("Quantum-Test");

    let concept = ontology.create_superposition(
        "superpos-1".to_string(),
        vec![
            ("state-a".to_string(), 0.6),
            ("state-b".to_string(), 0.8),
        ],
    );

    ontology.add_concept(concept.clone()).await.unwrap();

    // Check that quantum state is normalized (approximately)
    if let Some(ref qstate) = concept.quantum_state {
        let total: f64 = qstate.states.iter().map(|(_, amp)| amp * amp).sum();
        assert!((total - 1.0).abs() < 0.01);
    } else {
        panic!("Quantum state not set");
    }
}

#[tokio::test]
async fn test_quantum_entanglement() {
    let mut ontology = QuantumOntology::new("Quantum-Test");

    let concept1 = ontology.create_superposition(
        "c1".to_string(),
        vec![("s1".to_string(), 1.0)],
    );
    let concept2 = ontology.create_superposition(
        "c2".to_string(),
        vec![("s2".to_string(), 1.0)],
    );

    let id1 = concept1.id;
    let id2 = concept2.id;

    ontology.add_concept(concept1).await.unwrap();
    ontology.add_concept(concept2).await.unwrap();

    ontology.entangle(id1, id2).unwrap();

    let concepts = ontology.query_concepts("*").await.unwrap();
    let c1 = concepts.iter().find(|c| c.id == id1).unwrap();

    assert!(c1.quantum_state.as_ref().unwrap().entangled_with.contains(&id2));
}

#[tokio::test]
async fn test_collective_ontology_consensus() {
    let mut ontology = CollectiveOntology::new("Collective-Test", 0.7);

    let node1 = ontology.add_node(0.9);
    let node2 = ontology.add_node(0.8);
    let node3 = ontology.add_node(0.85);

    ontology.connect_nodes(node1, node2).unwrap();
    ontology.connect_nodes(node2, node3).unwrap();

    let concept = Concept::new(
        "test-concept".to_string(),
        b"test data".to_vec(),
        ConceptType::Abstract,
    );

    let concept_id = concept.id;

    // Add concept to multiple nodes
    ontology.add_concept_to_node(concept.clone(), node1, 0.9).unwrap();
    ontology.add_concept_to_node(concept.clone(), node2, 0.85).unwrap();

    let consensus = ontology.get_consensus(concept_id).unwrap();
    assert!(consensus > 0.8); // High consensus due to high reliability nodes

    assert!(ontology.has_consensus(concept_id));
}

#[tokio::test]
async fn test_temporal_ontology_branches() {
    let mut ontology = TemporalOntology::new("Temporal-Test");

    let main_timeline = ontology.current_timeline().unwrap().id;

    // Create a branch
    let branch = ontology.create_branch(
        main_timeline,
        vec![0.0, 1.0, 0.0],
        0.5,
    );

    ontology.switch_timeline(branch).unwrap();

    let concept = ontology.create_temporal_concept(
        "event-1".to_string(),
        vec![1.0, 2.0, 3.0],
        Some(vec![1.0, 0.0, 0.0]),
    );

    ontology.add_concept(concept).await.unwrap();

    assert_eq!(ontology.timelines().len(), 2);
}

#[tokio::test]
async fn test_validation() {
    let mut ontology = CrystallineOntology::new("Test");

    let c1 = Concept::new("c1".to_string(), vec![1, 2, 3], ConceptType::Entity);
    let c2 = Concept::new("c2".to_string(), vec![4, 5, 6], ConceptType::Entity);

    let id1 = c1.id;
    let id2 = c2.id;

    ontology.add_concept(c1).await.unwrap();
    ontology.add_concept(c2).await.unwrap();

    // Add valid relation
    let relation = Relation::new(
        RelationType::Similarity,
        id1,
        id2,
        0.8,
        false,
    );

    ontology.add_relation(relation).await.unwrap();

    let report = ontology.validate().await.unwrap();
    assert!(report.valid);
    assert_eq!(report.errors.len(), 0);
}

#[tokio::test]
async fn test_compatibility_score() {
    let crystalline = CrystallineOntology::new("Cryst");
    let quantum = QuantumOntology::new("Quant");

    let score = crystalline.compatibility_score(&quantum).await.unwrap();

    // Crystalline and Quantum have some compatibility (0.4)
    assert!(score > 0.3 && score < 0.5);
}

#[tokio::test]
async fn test_serialization() {
    let mut ontology = CrystallineOntology::new("Test");

    let concept = Concept::new(
        "test".to_string(),
        vec![1, 2, 3],
        ConceptType::Entity,
    );

    ontology.add_concept(concept).await.unwrap();

    let serialized = ontology.serialize().await.unwrap();
    assert!(!serialized.is_empty());

    let deserialized = CrystallineOntology::deserialize(&serialized).await.unwrap();
    assert_eq!(deserialized.id(), ontology.id());
}

#[test]
fn test_artifact_creation() {
    let data = vec![1, 2, 3, 4, 5];
    let artifact = Artifact::new(data.clone(), "Test Location".to_string());

    assert_eq!(artifact.size(), 5);
    assert_eq!(artifact.discovery.location, "Test Location");
    assert_eq!(artifact.decoding_progress, 0.0);
}

#[test]
fn test_archaeology_mission() {
    let mut mission = ArchaeologyMission::new(
        "Test Mission".to_string(),
        "Mars".to_string(),
    );

    assert_eq!(mission.status, MissionStatus::Planning);

    mission.add_objective("Find artifacts".to_string());
    mission.update_status(MissionStatus::InProgress);

    assert_eq!(mission.status, MissionStatus::InProgress);
    assert_eq!(mission.objectives.len(), 1);
}

#[test]
fn test_statistical_analyzer() {
    let analyzer = StatisticalAnalyzer::new();

    // Test entropy calculation
    let data = vec![0u8; 100]; // All zeros - low entropy
    let entropy = analyzer.entropy(&data);
    assert!(entropy < 1.0);

    let random_data: Vec<u8> = (0..100).map(|i| (i * 7) as u8).collect();
    let random_entropy = analyzer.entropy(&random_data);
    assert!(random_entropy > entropy);
}

#[test]
fn test_computational_paradigm_properties() {
    let quantum = ComputationalParadigm::Quantum;

    assert_eq!(quantum.complexity_class(), "BQP");
    assert!(quantum.energy_efficiency() < 1.0);
    assert!(quantum.information_density() > 1e15);
}

#[test]
fn test_harmonic_similarity() {
    let ontology = CrystallineOntology::new("Test");

    let c1 = ontology.create_pattern_concept(
        "c1".to_string(),
        vec![440.0, 550.0],
    );

    let c2 = ontology.create_pattern_concept(
        "c2".to_string(),
        vec![440.0, 550.0], // Same frequencies
    );

    let similarity = ontology.harmonic_similarity(&c1, &c2);
    assert!(similarity > 0.9); // Should be very similar
}

#[tokio::test]
async fn test_temporal_distance() {
    let ontology = TemporalOntology::new("Test");

    let c1 = ontology.create_temporal_concept(
        "e1".to_string(),
        vec![0.0, 0.0, 0.0],
        None,
    );

    let c2 = ontology.create_temporal_concept(
        "e2".to_string(),
        vec![3.0, 4.0, 0.0],
        None,
    );

    let distance = ontology.temporal_distance(&c1, &c2);
    assert!((distance - 5.0).abs() < 0.01); // 3-4-5 triangle
}
