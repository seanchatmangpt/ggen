//! μ₃ (Emit) Unit Tests
//!
//! Chicago TDD: AAA pattern, real collaborators, state-based verification

use ggen_craftplan::emit::{Emitter, GenerationConfig};
use ggen_craftplan::extract::{Attribute, Entity};
use chicago_tdd_tools::prelude::*;

test!(test_emitter_creation, {
    // Arrange & Act
    let emitter = Emitter::new();

    // Assert
    assert!(emitter.is_ok(), "Emitter should be created successfully");
});

test!(test_emitter_default, {
    // Arrange & Act
    let emitter = Emitter::default();

    // Assert
    assert!(emitter.tera.get_template_names().count() > 0, "Should have templates loaded");
});

test!(test_emit_ash_resource_minimal, {
    // Arrange
    let emitter = Emitter::new().expect("Failed to create emitter");
    let entity = Entity {
        name: "Product".to_string(),
        plural: Some("Products".to_string()),
        attributes: vec![],
        relationships: vec![],
    };
    let config = GenerationConfig {
        app_name: "Craftplan".to_string(),
        module_prefix: "Craftplan".to_string(),
        version: "0.1.0".to_string(),
    };

    // Act
    let rendered = emitter.render_ash_resource(&entity, &config);

    // Assert
    assert!(rendered.is_ok(), "Should render Ash resource successfully");
    let code = rendered.unwrap();
    assert!(code.contains("defmodule"), "Should contain defmodule");
    assert!(code.contains("Craftplan.Product"), "Should have module name");
    assert!(code.contains("Ash.Resource"), "Should use Ash.Resource");
});

test!(test_emit_ash_resource_with_attributes, {
    // Arrange
    let emitter = Emitter::new().expect("Failed to create emitter");
    let entity = Entity {
        name: "Product".to_string(),
        plural: Some("Products".to_string()),
        attributes: vec![
            Attribute {
                name: "name".to_string(),
                type_: "string".to_string(),
                required: true,
                doc: Some("Product name".to_string()),
            },
            Attribute {
                name: "price".to_string(),
                type_: "decimal".to_string(),
                required: false,
                doc: None,
            },
        ],
        relationships: vec![],
    };
    let config = GenerationConfig {
        app_name: "Craftplan".to_string(),
        module_prefix: "Craftplan".to_string(),
        version: "0.1.0".to_string(),
    };

    // Act
    let rendered = emitter.render_ash_resource(&entity, &config);

    // Assert
    assert!(rendered.is_ok(), "Should render with attributes");
    let code = rendered.unwrap();
    assert!(code.contains("attribute :name, :string"), "Should have name attribute");
    assert!(code.contains("attribute :price, :decimal"), "Should have price attribute");
    assert!(code.contains("Product name"), "Should include documentation");
});

test!(test_emit_context_module, {
    // Arrange
    let emitter = Emitter::new().expect("Failed to create emitter");
    let entities = vec![
        Entity {
            name: "Product".to_string(),
            plural: Some("Products".to_string()),
            attributes: vec![],
            relationships: vec![],
        },
        Entity {
            name: "Order".to_string(),
            plural: Some("Orders".to_string()),
            attributes: vec![],
            relationships: vec![],
        },
    ];
    let config = GenerationConfig {
        app_name: "Craftplan".to_string(),
        module_prefix: "Craftplan".to_string(),
        version: "0.1.0".to_string(),
    };

    // Act
    let rendered = emitter.render_context("Craftplan", &entities, &config);

    // Assert
    assert!(rendered.is_ok(), "Should render context module");
    let code = rendered.unwrap();
    assert!(code.contains("defmodule Craftplan do"), "Should have module definition");
    assert!(code.contains("list_product_all"), "Should have Product accessor");
    assert!(code.contains("list_order_all"), "Should have Order accessor");
});

test!(test_emit_live_view, {
    // Arrange
    let emitter = Emitter::new().expect("Failed to create emitter");
    let entity = Entity {
        name: "Product".to_string(),
        plural: Some("Products".to_string()),
        attributes: vec![
            Attribute {
                name: "name".to_string(),
                type_: "string".to_string(),
                required: true,
                doc: None,
            },
            Attribute {
                name: "price".to_string(),
                type_: "decimal".to_string(),
                required: false,
                doc: None,
            },
        ],
        relationships: vec![],
    };
    let config = GenerationConfig {
        app_name: "Craftplan".to_string(),
        module_prefix: "Craftplan".to_string(),
        version: "0.1.0".to_string(),
    };

    // Act
    let rendered = emitter.render_live_view(&entity, &config);

    // Assert
    assert!(rendered.is_ok(), "Should render LiveView");
    let code = rendered.unwrap();
    assert!(code.contains("defmodule CraftplanWeb.Live.ProductIndex"), "Should have LiveView module");
    assert!(code.contains("use CraftplanWeb, :live_view"), "Should use live_view");
});

test!(test_emit_output_is_deterministic, {
    // Arrange
    let emitter = Emitter::new().expect("Failed to create emitter");
    let entity = Entity {
        name: "Product".to_string(),
        plural: None,
        attributes: vec![],
        relationships: vec![],
    };
    let config = GenerationConfig {
        app_name: "Craftplan".to_string(),
        module_prefix: "Craftplan".to_string(),
        version: "0.1.0".to_string(),
    };

    // Act
    let rendered1 = emitter.render_ash_resource(&entity, &config);
    let rendered2 = emitter.render_ash_resource(&entity, &config);

    // Assert
    assert!(rendered1.is_ok(), "First render should succeed");
    assert!(rendered2.is_ok(), "Second render should succeed");
    let code1 = rendered1.unwrap();
    let code2 = rendered2.unwrap();
    assert_eq!(code1, code2, "Same input should produce identical output");
});
