use ggen_graph::DeterministicGraph;
use std::error::Error;

#[test]
fn test_hash_stability_order_independence() -> Result<(), Box<dyn Error>> {
    let q1 = "<http://example.org/a> <http://example.org/p> \"1\" .";
    let q2 = "<http://example.org/b> <http://example.org/p> \"2\" .";
    let q3 = "<http://example.org/c> <http://example.org/p> \"3\" .";

    let quad1 = DeterministicGraph::parse_nquad(q1)?;
    let quad2 = DeterministicGraph::parse_nquad(q2)?;
    let quad3 = DeterministicGraph::parse_nquad(q3)?;

    // Insert order 1
    let graph1 = DeterministicGraph::new()?;
    graph1.insert_quad(&quad1)?;
    graph1.insert_quad(&quad2)?;
    graph1.insert_quad(&quad3)?;
    let hash1 = graph1.state_hash()?;

    // Insert order 2
    let graph2 = DeterministicGraph::new()?;
    graph2.insert_quad(&quad3)?;
    graph2.insert_quad(&quad1)?;
    graph2.insert_quad(&quad2)?;
    let hash2 = graph2.state_hash()?;

    // Assert that hashes are identical
    assert_eq!(hash1, hash2);

    // Remove one quad and verify the hash changes
    graph1.remove_quad(&quad2)?;
    let hash3 = graph1.state_hash()?;
    assert_ne!(hash1, hash3);

    // Insert it back and verify it returns to the same hash
    graph1.insert_quad(&quad2)?;
    let hash4 = graph1.state_hash()?;
    assert_eq!(hash1, hash4);

    Ok(())
}
