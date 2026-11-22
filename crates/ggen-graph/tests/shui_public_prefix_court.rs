//! Chicago court: W3C SHACL 1.2 `shui:` is an admitted public Turtle prefix.
//!
//! ECOSYSTEM-SPEC-27.1.1 Gate 1 foundation — AAA, no mocks.
//! Falsifier: `cargo test -p ggen-graph --test shui_public_prefix_court`

use ggen_graph::vocab::{
    is_admitted_turtle_prefix, is_open_admitted_namespace_iri, is_refused_private_prefix, shui,
    ADMITTED_TURTLE_PREFIXES,
};

#[test]
fn shui_namespace_is_w3c_shacl_ui_slash_iri() {
    // Arrange
    let expected = "http://www.w3.org/ns/shacl-ui/";

    // Act
    let actual = shui::NAMESPACE;
    let prefix = shui::PREFIX;

    // Assert
    assert_eq!(actual, expected);
    assert_eq!(prefix, "shui");
    assert!(
        !actual.ends_with('#'),
        "shui: must use the slash IRI form per W3C SHACL 1.2 UI"
    );
}

#[test]
fn shui_prefix_is_admitted_and_private_prefixes_remain_refused() {
    // Arrange / Act / Assert — admission
    assert!(
        is_admitted_turtle_prefix("shui"),
        "shui must be in the public Turtle prefix allowlist"
    );
    assert!(
        ADMITTED_TURTLE_PREFIXES.contains(&"shui"),
        "shui must appear in ADMITTED_TURTLE_PREFIXES"
    );
    assert!(
        is_admitted_turtle_prefix("sh"),
        "baseline sh: must remain admitted"
    );

    // Assert — private tokens stay refused (do not conflate truex: ban with shui:)
    for banned in ["truex", "gall", "gg", "kh", "pcst"] {
        assert!(
            is_refused_private_prefix(banned),
            "{banned} must remain a refused private prefix"
        );
        assert!(
            !is_admitted_turtle_prefix(banned),
            "{banned} must not be admitted as a public Turtle prefix"
        );
    }
}

#[test]
fn shui_iris_are_open_admitted_public_namespace() {
    // Arrange
    let widget = "http://www.w3.org/ns/shacl-ui/Widget";
    let private_lookalike = "http://example.org/truex/Widget";

    // Act / Assert
    assert!(is_open_admitted_namespace_iri(widget));
    assert!(shui::is_shui_iri(widget));
    assert!(!is_open_admitted_namespace_iri(private_lookalike));
    assert!(!shui::is_shui_iri("http://www.w3.org/ns/shacl#NodeShape"));
}
