//! Chicago TDD Schema Type Tests
//!
//! Tests observable behavior of schema types and their conversions.
//! Focus: Type invariants, cardinality bounds, property range conversions
//!
//! AAA Pattern: Arrange (create types) -> Act (call methods) -> Assert (verify state/behavior)

use ggen_core::ontology::{
    Cardinality, OntClass, OntProperty, OntRelationship, OntologySchema, OwlRestriction,
    PropertyRange, RelationshipType,
};

// ============================================================================
// TEST GROUP: Cardinality Type Behavior
// ============================================================================

/// ARRANGE: Create different Cardinality variants
/// ACT: Call min() method
/// ASSERT: Returns correct minimum values (observable type state)
#[test]
fn cardinality_min_returns_correct_bounds() {
    // Arrange
    let one = Cardinality::One;
    let zero_or_one = Cardinality::ZeroOrOne;
    let many = Cardinality::Many;
    let one_or_more = Cardinality::OneOrMore;
    let range = Cardinality::Range {
        min: 3,
        max: Some(10),
    };

    // Act and Assert: Observable state - correct min values
    assert_eq!(one.min(), 1, "One cardinality min should be 1");
    assert_eq!(
        zero_or_one.min(),
        0,
        "ZeroOrOne cardinality min should be 0"
    );
    assert_eq!(many.min(), 0, "Many cardinality min should be 0");
    assert_eq!(
        one_or_more.min(),
        1,
        "OneOrMore cardinality min should be 1"
    );
    assert_eq!(range.min(), 3, "Range cardinality min should be 3");
}

/// ARRANGE: Create different Cardinality variants
/// ACT: Call max() method
/// ASSERT: Returns correct maximum values or None for unbounded
#[test]
fn cardinality_max_returns_correct_bounds() {
    // Arrange
    let one = Cardinality::One;
    let zero_or_one = Cardinality::ZeroOrOne;
    let many = Cardinality::Many;
    let one_or_more = Cardinality::OneOrMore;
    let range = Cardinality::Range {
        min: 2,
        max: Some(5),
    };

    // Act and Assert: Observable state - correct max values
    assert_eq!(one.max(), Some(1), "One cardinality max should be Some(1)");
    assert_eq!(
        zero_or_one.max(),
        Some(1),
        "ZeroOrOne cardinality max should be Some(1)"
    );
    assert_eq!(many.max(), None, "Many cardinality max should be None");
    assert_eq!(
        one_or_more.max(),
        None,
        "OneOrMore cardinality max should be None"
    );
    assert_eq!(
        range.max(),
        Some(5),
        "Range cardinality max should be Some(5)"
    );
}

/// ARRANGE: Create Cardinality variants
/// ACT: Call is_multi_valued()
/// ASSERT: Correctly identifies single vs multi-valued properties
#[test]
fn cardinality_multi_valued_check_is_correct() {
    // Arrange and Act
    let single_valued_types = vec![Cardinality::One, Cardinality::ZeroOrOne];
    let multi_valued_types = vec![
        Cardinality::Many,
        Cardinality::OneOrMore,
        Cardinality::Range { min: 0, max: None },
        Cardinality::Range {
            min: 1,
            max: Some(10),
        },
    ];

    // Assert: Observable state - correct classification
    for card in single_valued_types {
        assert!(
            !card.is_multi_valued(),
            "{:?} should be single-valued",
            card
        );
    }

    for card in multi_valued_types {
        assert!(card.is_multi_valued(), "{:?} should be multi-valued", card);
    }
}

/// ARRANGE: Create unbounded Range cardinality
/// ACT: Call max()
/// ASSERT: Returns None for unbounded (observable state)
#[test]
fn cardinality_range_unbounded_max_is_none() {
    // Arrange
    let unbounded = Cardinality::Range { min: 5, max: None };

    // Act and Assert
    assert_eq!(
        unbounded.max(),
        None,
        "Unbounded range should have max = None"
    );
    assert!(
        unbounded.is_multi_valued(),
        "Unbounded range should be multi-valued"
    );
}

// ============================================================================
// TEST GROUP: PropertyRange Type Conversions
// ============================================================================

/// ARRANGE: Create all PropertyRange variants
/// ACT: Call to_typescript_type()
/// ASSERT: Correct TypeScript type strings (observable state)
#[test]
fn property_range_typescript_conversion_is_deterministic() {
    // Arrange
    let test_cases = vec![
        (PropertyRange::String, "string"),
        (PropertyRange::Integer, "number"),
        (PropertyRange::Float, "number"),
        (PropertyRange::Boolean, "boolean"),
        (PropertyRange::DateTime, "Date"),
        (PropertyRange::Date, "string"),
        (PropertyRange::Time, "string"),
    ];

    // Act and Assert: Observable state - correct conversions
    for (range, expected) in test_cases {
        assert_eq!(
            range.to_typescript_type(),
            expected,
            "TypeScript conversion for {:?} should be '{}'",
            range,
            expected
        );
    }
}

/// ARRANGE: Create Reference PropertyRange
/// ACT: Call to_typescript_type()
/// ASSERT: Extracts local name from URI (observable behavior)
#[test]
fn property_range_reference_extracts_local_name_for_typescript() {
    // Arrange
    let product_ref = PropertyRange::Reference("http://example.org/schema#Product".to_string());
    let order_ref = PropertyRange::Reference("http://example.org/schema#Order".to_string());

    // Act and Assert: Observable state - URI parsed to class name
    assert_eq!(
        product_ref.to_typescript_type(),
        "Product",
        "Should extract Product from URI"
    );
    assert_eq!(
        order_ref.to_typescript_type(),
        "Order",
        "Should extract Order from URI"
    );
}

/// ARRANGE: Create Enum PropertyRange
/// ACT: Call to_typescript_type()
/// ASSERT: Generates union type (observable string state)
#[test]
fn property_range_enum_generates_union_type() {
    // Arrange
    let enum_range = PropertyRange::Enum(vec![
        "pending".to_string(),
        "processing".to_string(),
        "delivered".to_string(),
    ]);

    // Act
    let ts_type = enum_range.to_typescript_type();

    // Assert: Observable state - union type generated
    assert_eq!(
        ts_type, "'pending' | 'processing' | 'delivered'",
        "Should generate TypeScript union type"
    );
}

/// ARRANGE: Create all PropertyRange variants
/// ACT: Call to_sql_type()
/// ASSERT: Correct SQL type strings (observable state)
#[test]
fn property_range_sql_conversion_is_deterministic() {
    // Arrange
    let test_cases = vec![
        (PropertyRange::String, "VARCHAR(255)"),
        (PropertyRange::Integer, "INTEGER"),
        (PropertyRange::Float, "DECIMAL(10,2)"),
        (PropertyRange::Boolean, "BOOLEAN"),
        (PropertyRange::DateTime, "TIMESTAMPTZ"),
        (PropertyRange::Date, "DATE"),
        (PropertyRange::Time, "TIME"),
    ];

    // Act and Assert: Observable state - correct SQL types
    for (range, expected) in test_cases {
        assert_eq!(
            range.to_sql_type(),
            expected,
            "SQL conversion for {:?} should be '{}'",
            range,
            expected
        );
    }
}

/// ARRANGE: Create Reference PropertyRange
/// ACT: Call to_sql_type()
/// ASSERT: Generates UUID type (observable state)
#[test]
fn property_range_reference_generates_uuid_for_sql() {
    // Arrange
    let ref_range = PropertyRange::Reference("http://example.org/schema#User".to_string());

    // Act and Assert
    assert_eq!(
        ref_range.to_sql_type(),
        "UUID",
        "Reference should map to UUID in SQL"
    );
}

/// ARRANGE: Create Literal PropertyRange with "json"
/// ACT: Call to_sql_type()
/// ASSERT: Generates JSONB type (observable state)
#[test]
fn property_range_literal_json_generates_jsonb() {
    // Arrange
    let json_literal = PropertyRange::Literal("json".to_string());
    let custom_literal = PropertyRange::Literal("xml".to_string());

    // Act and Assert
    assert_eq!(
        json_literal.to_sql_type(),
        "JSONB",
        "JSON literal should map to JSONB"
    );
    assert_eq!(
        custom_literal.to_sql_type(),
        "VARCHAR(255)",
        "Unknown literal should map to VARCHAR"
    );
}

/// ARRANGE: Create Enum PropertyRange
/// ACT: Call to_sql_type()
/// ASSERT: Generates VARCHAR for enum (observable state)
#[test]
fn property_range_enum_generates_varchar_for_sql() {
    // Arrange
    let status_enum = PropertyRange::Enum(vec!["active".to_string(), "inactive".to_string()]);

    // Act and Assert
    assert_eq!(
        status_enum.to_sql_type(),
        "VARCHAR(50)",
        "Enum should map to VARCHAR(50) in SQL"
    );
}

// ============================================================================
// TEST GROUP: PropertyRange GraphQL Conversions
// ============================================================================

/// ARRANGE: Create PropertyRange with different Cardinality
/// ACT: Call to_graphql_type()
/// ASSERT: Generates correct GraphQL type strings with null modifiers
#[test]
fn property_range_graphql_respects_cardinality() {
    // Arrange
    let string_type = PropertyRange::String;

    // Act and Assert: Observable state - GraphQL types with cardinality
    assert_eq!(
        string_type.to_graphql_type(&Cardinality::One),
        "String!",
        "Required single string should be String!"
    );

    assert_eq!(
        string_type.to_graphql_type(&Cardinality::ZeroOrOne),
        "String",
        "Optional single string should be String"
    );

    assert_eq!(
        string_type.to_graphql_type(&Cardinality::Many),
        "[String]!",
        "Many strings should be [String]!"
    );

    assert_eq!(
        string_type.to_graphql_type(&Cardinality::OneOrMore),
        "[String]!",
        "OneOrMore should be [String]!"
    );
}

/// ARRANGE: Create Reference PropertyRange with different Cardinality
/// ACT: Call to_graphql_type()
/// ASSERT: Generates correct object type references (observable behavior)
#[test]
fn property_range_reference_graphql_includes_class_name() {
    // Arrange
    let user_ref = PropertyRange::Reference("http://example.org/schema#User".to_string());

    // Act and Assert: Observable state - class name extracted
    assert_eq!(
        user_ref.to_graphql_type(&Cardinality::One),
        "User!",
        "Required User reference should be User!"
    );

    assert_eq!(
        user_ref.to_graphql_type(&Cardinality::Many),
        "[User]!",
        "Many User references should be [User]!"
    );
}

/// ARRANGE: Create Enum PropertyRange
/// ACT: Call to_graphql_type()
/// ASSERT: Generates enum type (observable string state)
#[test]
fn property_range_enum_graphql_uses_enum_syntax() {
    // Arrange
    let status_enum = PropertyRange::Enum(vec![
        "ACTIVE".to_string(),
        "INACTIVE".to_string(),
        "ARCHIVED".to_string(),
    ]);

    // Act and Assert
    assert_eq!(
        status_enum.to_graphql_type(&Cardinality::One),
        "ACTIVE | INACTIVE | ARCHIVED!",
        "Should generate GraphQL enum union"
    );
}

// ============================================================================
// TEST GROUP: OntClass Observable Behavior
// ============================================================================

/// ARRANGE: Create OntClass with various configurations
/// ACT: Examine class properties
/// ASSERT: All fields have expected values (observable state)
#[test]
fn ont_class_stores_all_defined_fields() {
    // Arrange
    let class = OntClass {
        uri: "http://example.org/schema#Product".to_string(),
        name: "Product".to_string(),
        label: "Product Class".to_string(),
        description: Some("A product in the catalog".to_string()),
        parent_classes: vec!["http://example.org/schema#Entity".to_string()],
        properties: vec!["productName".to_string(), "price".to_string()],
        is_abstract: false,
        restrictions: vec![
            OwlRestriction::MinCardinality(1),
            OwlRestriction::MaxCardinality(1),
        ],
    };

    // Act and Assert: Observable state - all fields present and correct
    assert_eq!(class.uri, "http://example.org/schema#Product");
    assert_eq!(class.name, "Product");
    assert_eq!(class.label, "Product Class");
    assert_eq!(
        class.description,
        Some("A product in the catalog".to_string())
    );
    assert_eq!(class.parent_classes.len(), 1);
    assert_eq!(class.properties.len(), 2);
    assert!(!class.is_abstract);
    assert_eq!(class.restrictions.len(), 2);
}

/// ARRANGE: Create abstract OntClass
/// ACT: Check is_abstract flag
/// ASSERT: Flag correctly reflects abstraction (observable state)
#[test]
fn ont_class_abstract_flag_is_observable() {
    // Arrange
    let concrete = OntClass {
        uri: "http://example.org/schema#Product".to_string(),
        name: "Product".to_string(),
        label: "Product".to_string(),
        description: None,
        parent_classes: vec![],
        properties: vec![],
        is_abstract: false,
        restrictions: vec![],
    };

    let abstract_class = OntClass {
        uri: "http://example.org/schema#Entity".to_string(),
        name: "Entity".to_string(),
        label: "Entity".to_string(),
        description: None,
        parent_classes: vec![],
        properties: vec![],
        is_abstract: true,
        restrictions: vec![],
    };

    // Act and Assert: Observable state - abstract flag correctly set
    assert!(!concrete.is_abstract);
    assert!(abstract_class.is_abstract);
}

// ============================================================================
// TEST GROUP: OntProperty Observable Behavior
// ============================================================================

/// ARRANGE: Create OntProperty with functional constraint
/// ACT: Check is_functional flag
/// ASSERT: Flag indicates single-valuedness (observable state)
#[test]
fn ont_property_functional_flag_indicates_single_valued() {
    // Arrange
    let single_valued = OntProperty {
        uri: "http://example.org/schema#productId".to_string(),
        name: "productId".to_string(),
        label: "Product ID".to_string(),
        description: Some("Unique product identifier".to_string()),
        domain: vec!["http://example.org/schema#Product".to_string()],
        range: PropertyRange::String,
        cardinality: Cardinality::One,
        required: true,
        is_functional: true,
        is_inverse_functional: false,
        inverse_of: None,
    };

    let multi_valued = OntProperty {
        uri: "http://example.org/schema#hasTag".to_string(),
        name: "hasTag".to_string(),
        label: "Has Tag".to_string(),
        description: None,
        domain: vec!["http://example.org/schema#Product".to_string()],
        range: PropertyRange::String,
        cardinality: Cardinality::Many,
        required: false,
        is_functional: false,
        is_inverse_functional: false,
        inverse_of: None,
    };

    // Act and Assert: Observable state - functional flag correctly indicates constraints
    assert!(single_valued.is_functional);
    assert!(!multi_valued.is_functional);
}

/// ARRANGE: Create OntProperty with inverse property
/// ACT: Check inverse_of field
/// ASSERT: Bidirectional relationship is observable (state)
#[test]
fn ont_property_inverse_relationship_is_observable() {
    // Arrange
    let has_author = OntProperty {
        uri: "http://example.org/schema#hasAuthor".to_string(),
        name: "hasAuthor".to_string(),
        label: "Has Author".to_string(),
        description: None,
        domain: vec!["http://example.org/schema#Book".to_string()],
        range: PropertyRange::Reference("http://example.org/schema#Person".to_string()),
        cardinality: Cardinality::ZeroOrOne,
        required: false,
        is_functional: true,
        is_inverse_functional: false,
        inverse_of: Some("http://example.org/schema#authorOf".to_string()),
    };

    // Act and Assert: Observable state - inverse relationship recorded
    assert_eq!(
        has_author.inverse_of,
        Some("http://example.org/schema#authorOf".to_string())
    );
}

// ============================================================================
// TEST GROUP: OntRelationship Observable Behavior
// ============================================================================

/// ARRANGE: Create OntRelationship with different relationship types
/// ACT: Create relationships
/// ASSERT: Types correctly represent cardinality (observable state)
#[test]
fn ont_relationship_types_represent_cardinality() {
    // Arrange and Act
    let one_to_one = OntRelationship {
        from_class: "http://example.org/schema#Person".to_string(),
        to_class: "http://example.org/schema#Passport".to_string(),
        property: "hasPassport".to_string(),
        relationship_type: RelationshipType::OneToOne,
        bidirectional: false,
        label: "Has Passport".to_string(),
    };

    let one_to_many = OntRelationship {
        from_class: "http://example.org/schema#Author".to_string(),
        to_class: "http://example.org/schema#Book".to_string(),
        property: "hasBooks".to_string(),
        relationship_type: RelationshipType::OneToMany,
        bidirectional: false,
        label: "Has Books".to_string(),
    };

    let many_to_many = OntRelationship {
        from_class: "http://example.org/schema#Student".to_string(),
        to_class: "http://example.org/schema#Course".to_string(),
        property: "enrolledIn".to_string(),
        relationship_type: RelationshipType::ManyToMany,
        bidirectional: true,
        label: "Enrolled In".to_string(),
    };

    // Assert: Observable state - types correctly represent relationships
    assert_eq!(one_to_one.relationship_type, RelationshipType::OneToOne);
    assert_eq!(one_to_many.relationship_type, RelationshipType::OneToMany);
    assert_eq!(many_to_many.relationship_type, RelationshipType::ManyToMany);
    assert!(!one_to_one.bidirectional);
    assert!(many_to_many.bidirectional);
}

/// ARRANGE: Create bidirectional OntRelationship
/// ACT: Check bidirectional flag
/// ASSERT: Flag indicates two-way relationship (observable state)
#[test]
fn ont_relationship_bidirectional_flag_is_observable() {
    // Arrange
    let unidirectional = OntRelationship {
        from_class: "Book".to_string(),
        to_class: "Author".to_string(),
        property: "writtenBy".to_string(),
        relationship_type: RelationshipType::ManyToOne,
        bidirectional: false,
        label: "Written By".to_string(),
    };

    let bidirectional = OntRelationship {
        from_class: "Person".to_string(),
        to_class: "Friend".to_string(),
        property: "hasFriend".to_string(),
        relationship_type: RelationshipType::ManyToMany,
        bidirectional: true,
        label: "Has Friend".to_string(),
    };

    // Act and Assert: Observable state - bidirectional flag correct
    assert!(!unidirectional.bidirectional);
    assert!(bidirectional.bidirectional);
}

// ============================================================================
// TEST GROUP: OntologySchema Navigation
// ============================================================================

/// ARRANGE: Create OntologySchema with sample data
/// ACT: Call find_class()
/// ASSERT: Returns correct class or None (observable navigation)
#[test]
fn ont_schema_find_class_by_name_returns_correct_result() {
    // Arrange
    let schema = OntologySchema {
        classes: vec![
            OntClass {
                uri: "http://example.org#Product".to_string(),
                name: "Product".to_string(),
                label: "Product".to_string(),
                description: None,
                parent_classes: vec![],
                properties: vec![],
                is_abstract: false,
                restrictions: vec![],
            },
            OntClass {
                uri: "http://example.org#Order".to_string(),
                name: "Order".to_string(),
                label: "Order".to_string(),
                description: None,
                parent_classes: vec![],
                properties: vec![],
                is_abstract: false,
                restrictions: vec![],
            },
        ],
        properties: vec![],
        relationships: vec![],
        namespace: "http://example.org".to_string(),
        version: "1.0.0".to_string(),
        label: "Test Schema".to_string(),
        description: None,
        metadata: Default::default(),
    };

    // Act and Assert: Observable state - navigation works
    assert!(schema.find_class("Product").is_some());
    assert!(schema.find_class("Order").is_some());
    assert!(schema.find_class("NonExistent").is_none());
}

/// ARRANGE: Create OntologySchema with properties
/// ACT: Call find_property()
/// ASSERT: Returns correct property or None (observable navigation)
#[test]
fn ont_schema_find_property_by_name_returns_correct_result() {
    // Arrange
    let schema = OntologySchema {
        classes: vec![],
        properties: vec![
            OntProperty {
                uri: "http://example.org#name".to_string(),
                name: "name".to_string(),
                label: "Name".to_string(),
                description: None,
                domain: vec![],
                range: PropertyRange::String,
                cardinality: Cardinality::One,
                required: false,
                is_functional: false,
                is_inverse_functional: false,
                inverse_of: None,
            },
            OntProperty {
                uri: "http://example.org#price".to_string(),
                name: "price".to_string(),
                label: "Price".to_string(),
                description: None,
                domain: vec![],
                range: PropertyRange::Float,
                cardinality: Cardinality::One,
                required: false,
                is_functional: false,
                is_inverse_functional: false,
                inverse_of: None,
            },
        ],
        relationships: vec![],
        namespace: "http://example.org".to_string(),
        version: "1.0.0".to_string(),
        label: "Test Schema".to_string(),
        description: None,
        metadata: Default::default(),
    };

    // Act and Assert: Observable state - property navigation works
    assert!(schema.find_property("name").is_some());
    assert!(schema.find_property("price").is_some());
    assert!(schema.find_property("nonExistent").is_none());
}

/// ARRANGE: Create OntologySchema with class having properties
/// ACT: Call properties_for_class()
/// ASSERT: Returns filtered properties for domain (observable state)
#[test]
fn ont_schema_properties_for_class_filters_correctly() {
    // Arrange
    let product_uri = "http://example.org#Product";
    let order_uri = "http://example.org#Order";

    let schema = OntologySchema {
        classes: vec![],
        properties: vec![
            OntProperty {
                uri: "http://example.org#productName".to_string(),
                name: "productName".to_string(),
                label: "Product Name".to_string(),
                description: None,
                domain: vec![product_uri.to_string()],
                range: PropertyRange::String,
                cardinality: Cardinality::One,
                required: false,
                is_functional: false,
                is_inverse_functional: false,
                inverse_of: None,
            },
            OntProperty {
                uri: "http://example.org#price".to_string(),
                name: "price".to_string(),
                label: "Price".to_string(),
                description: None,
                domain: vec![product_uri.to_string()],
                range: PropertyRange::Float,
                cardinality: Cardinality::One,
                required: false,
                is_functional: false,
                is_inverse_functional: false,
                inverse_of: None,
            },
            OntProperty {
                uri: "http://example.org#orderId".to_string(),
                name: "orderId".to_string(),
                label: "Order ID".to_string(),
                description: None,
                domain: vec![order_uri.to_string()],
                range: PropertyRange::String,
                cardinality: Cardinality::One,
                required: false,
                is_functional: false,
                is_inverse_functional: false,
                inverse_of: None,
            },
        ],
        relationships: vec![],
        namespace: "http://example.org".to_string(),
        version: "1.0.0".to_string(),
        label: "Test Schema".to_string(),
        description: None,
        metadata: Default::default(),
    };

    // Act: Get properties for Product class
    let product_props = schema.properties_for_class(product_uri);
    let order_props = schema.properties_for_class(order_uri);

    // Assert: Observable state - correctly filtered by domain
    assert_eq!(product_props.len(), 2, "Product should have 2 properties");
    assert_eq!(order_props.len(), 1, "Order should have 1 property");

    let product_names: Vec<_> = product_props.iter().map(|p| p.name.as_str()).collect();
    assert!(product_names.contains(&"productName"));
    assert!(product_names.contains(&"price"));
    assert!(!product_names.contains(&"orderId"));
}
