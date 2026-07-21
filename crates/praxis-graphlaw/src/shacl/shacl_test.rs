/// SHACL module tests
///
/// Tests for core SHACL types and functionality.

#[cfg(test)]
mod tests {
    use super::super::closure::ClosureMatrix;
    use super::super::model::{
        CompiledConstraint, CompiledShape, CompiledTarget, CostClass, PropertyMask, TargetType,
        SHACL_SPARQL_BOUNDARY,
    };

    #[test]
    fn test_cost_class_ordering() {
        // Verify that CostClass enum has correct ordering:
        // Cardinality < NodeKind < Datatype < Class < Path < Regex < Recursive
        assert!(CostClass::Cardinality < CostClass::NodeKind);
        assert!(CostClass::NodeKind < CostClass::Datatype);
        assert!(CostClass::Datatype < CostClass::Class);
        assert!(CostClass::Class < CostClass::Path);
        assert!(CostClass::Path < CostClass::Regex);
        assert!(CostClass::Regex < CostClass::Recursive);
    }

    #[test]
    fn test_compiled_constraint_creation() {
        // Test that CompiledConstraint can be created with all cost classes
        let constraints = [
            CompiledConstraint {
                cost_class: CostClass::Cardinality,
                predicate: 1,
                value: 5,
                is_optional: false,
            },
            CompiledConstraint {
                cost_class: CostClass::NodeKind,
                predicate: 2,
                value: 10,
                is_optional: true,
            },
            CompiledConstraint {
                cost_class: CostClass::Class,
                predicate: 3,
                value: 15,
                is_optional: false,
            },
        ];

        assert_eq!(constraints.len(), 3);
        assert_eq!(constraints[0].cost_class, CostClass::Cardinality);
        assert_eq!(constraints[1].is_optional, true);
        assert_eq!(constraints[2].cost_class, CostClass::Class);
    }

    #[test]
    fn test_constraint_sorting() {
        // Verify that constraints can be sorted by CostClass
        let mut constraints = [
            CompiledConstraint {
                cost_class: CostClass::Recursive,
                predicate: 1,
                value: 1,
                is_optional: false,
            },
            CompiledConstraint {
                cost_class: CostClass::Cardinality,
                predicate: 2,
                value: 2,
                is_optional: false,
            },
            CompiledConstraint {
                cost_class: CostClass::Path,
                predicate: 3,
                value: 3,
                is_optional: false,
            },
        ];

        // Sort by cost class
        constraints.sort_by_key(|c| c.cost_class);

        assert_eq!(constraints[0].cost_class, CostClass::Cardinality);
        assert_eq!(constraints[1].cost_class, CostClass::Path);
        assert_eq!(constraints[2].cost_class, CostClass::Recursive);
    }

    #[test]
    fn test_shacl_sparql_boundary() {
        // Test that SHACL-SPARQL boundary is set to CORE_ONLY
        assert_eq!(SHACL_SPARQL_BOUNDARY, "CORE_ONLY");
    }

    #[test]
    fn test_target_type_creation() {
        // All four TargetType variants are distinct values.
        let variants = [
            TargetType::Node,
            TargetType::Class,
            TargetType::SubjectsOf,
            TargetType::ObjectsOf,
        ];
        for (i, a) in variants.iter().enumerate() {
            for b in &variants[i + 1..] {
                assert_ne!(a, b, "TargetType variants must be distinct");
            }
        }
    }

    #[test]
    fn test_compiled_target_creation() {
        let target = CompiledTarget {
            target_value: 42,
            target_type: TargetType::Class,
        };
        assert_eq!(target.target_value, 42);
        assert_eq!(target.target_type, TargetType::Class);
    }

    #[test]
    fn test_compiled_shape_creation() {
        let shape = CompiledShape {
            iri: 1,
            targets: vec![CompiledTarget {
                target_value: 10,
                target_type: TargetType::Node,
            }],
            constraints: vec![CompiledConstraint {
                cost_class: CostClass::Cardinality,
                predicate: 2,
                value: 5,
                is_optional: false,
            }],
            closed: false,
            property_shapes: vec![],
            allowed_predicates: vec![],
            required_properties_mask: PropertyMask(0),
        };

        assert_eq!(shape.iri, 1);
        assert_eq!(shape.targets.len(), 1);
        assert_eq!(shape.constraints.len(), 1);
        assert_eq!(shape.closed, false);
        assert_eq!(shape.property_shapes.len(), 0);
    }

    #[test]
    fn test_closure_matrix_creation() {
        // PROJ-409: Test ClosureMatrix creation
        let matrix = ClosureMatrix::new(10);
        assert_eq!(matrix.max_id, 10);
    }

    #[test]
    fn test_closure_matrix_edge_addition() {
        // PROJ-409: Test adding edges to ClosureMatrix
        let mut matrix = ClosureMatrix::new(5);

        // Add edges: 0 -> 1, 1 -> 2
        matrix.add_edge(0, 1);
        matrix.add_edge(1, 2);

        // Check direct reachability
        assert!(matrix.is_reachable(0, 1));
        assert!(matrix.is_reachable(1, 2));
        assert!(!matrix.is_reachable(0, 2)); // Not yet, need transitive closure
    }

    #[test]
    fn test_closure_matrix_transitive_closure() {
        // PROJ-409: Test transitive closure computation
        let mut matrix = ClosureMatrix::new(5);

        // Create a simple chain: 0 -> 1 -> 2
        matrix.add_edge(0, 1);
        matrix.add_edge(1, 2);

        // Compute transitive closure
        matrix.compute_transitive_closure();

        // After closure, 0 should reach 2 transitively
        assert!(matrix.is_reachable(0, 1));
        assert!(matrix.is_reachable(0, 2)); // Now transitive
        assert!(matrix.is_reachable(1, 2));
    }

    #[test]
    fn test_closure_matrix_canonical_rendering() {
        // PROJ-409 Step 3: Test canonical rendering for deterministic hashing
        let mut matrix = ClosureMatrix::new(5);

        // Add some edges
        matrix.add_edge(1, 3);
        matrix.add_edge(0, 2);
        matrix.add_edge(2, 4);

        // Render canonical edges (should be sorted)
        let edges = matrix.render_canonical();

        // Edges should be in sorted order: (0,2), (1,3), (2,4)
        assert_eq!(edges.len(), 3);
        assert_eq!(edges[0], (0, 2));
        assert_eq!(edges[1], (1, 3));
        assert_eq!(edges[2], (2, 4));

        // Canonical rendering should be deterministic
        let edges2 = matrix.render_canonical();
        assert_eq!(edges, edges2);
    }

    #[test]
    fn test_closure_matrix_reachable_reference() {
        // PROJ-409: Test getting reachable nodes as reference
        let mut matrix = ClosureMatrix::new(5);
        matrix.add_edge(0, 1);
        matrix.add_edge(0, 2);

        // Get reachable set for node 0
        if let Some(reachable) = matrix.reachable(0) {
            assert!(reachable.contains(1));
            assert!(reachable.contains(2));
            assert!(!reachable.contains(3));
        } else {
            panic!("Expected Some for reachable(0)");
        }
    }

    #[test]
    fn test_closure_matrix_out_of_bounds() {
        // PROJ-409: Test that out-of-bounds node IDs are handled gracefully
        let mut matrix = ClosureMatrix::new(5);

        // Add edge within bounds
        matrix.add_edge(0, 2);

        // Check out-of-bounds access
        assert!(!matrix.is_reachable(0, 10)); // out of bounds
        assert!(!matrix.is_reachable(10, 0)); // out of bounds
        assert!(matrix.reachable(10).is_none());
    }
}
