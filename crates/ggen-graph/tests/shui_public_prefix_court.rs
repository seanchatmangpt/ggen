//! Chicago court: W3C SHACL 1.2 `shui:` is an admitted public Turtle prefix.
//!
//! ECOSYSTEM-SPEC-27.1.1 Gate 1 / Truex fence alignment — AAA, no mocks.
//! Falsifier: `cargo test -p ggen-graph --test shui_public_prefix_court`
//!
//! Claims proven here:
//! 1. `shui:` binds to `http://www.w3.org/ns/shacl-ui/` (slash IRI).
//! 2. `shui:` is ADMITTED; private `truex:`/`gall:`/… remain REFUSED_PRIVATE.
//! 3. Unknown Turtle prefixes are REFUSED_UNKNOWN (not silently admitted).
//! 4. UI property-group (`shui:parentGroup`) and editor/viewer IRIs are open-admitted.

use ggen_graph::vocab::{
    is_admitted_turtle_prefix, is_open_admitted_namespace_iri, is_refused_private_prefix, shui,
    turtle_prefix_standing, ADMITTED_TURTLE_PREFIXES, TurtlePrefixStanding,
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
    // Arrange / Act
    let shui_standing = turtle_prefix_standing("shui");
    let truex_standing = turtle_prefix_standing("truex");

    // Assert — admission
    assert_eq!(shui_standing, TurtlePrefixStanding::Admitted);
    assert_eq!(shui_standing.as_str(), "ADMITTED");
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
    assert_eq!(truex_standing, TurtlePrefixStanding::RefusedPrivate);
    assert_eq!(truex_standing.as_str(), "REFUSED_PRIVATE");
    for banned in ["truex", "gall", "gg", "kh", "pcst"] {
        assert!(
            is_refused_private_prefix(banned),
            "{banned} must remain a refused private prefix"
        );
        assert!(
            !is_admitted_turtle_prefix(banned),
            "{banned} must not be admitted as a public Turtle prefix"
        );
        assert_eq!(
            turtle_prefix_standing(banned),
            TurtlePrefixStanding::RefusedPrivate,
            "{banned} standing must be REFUSED_PRIVATE"
        );
    }
}

#[test]
fn unknown_turtle_prefix_is_refused_unknown() {
    // Arrange — tokens that are neither public nor on the private ban list
    let unknowns = ["acme", "dash", "foobarbaz", "privateish"];

    // Act / Assert
    for prefix in unknowns {
        let standing = turtle_prefix_standing(prefix);
        assert_eq!(
            standing,
            TurtlePrefixStanding::RefusedUnknown,
            "{prefix} must be REFUSED_UNKNOWN"
        );
        assert_eq!(standing.as_str(), "REFUSED_UNKNOWN");
        assert!(
            !standing.is_admitted(),
            "{prefix} must not be admitted"
        );
        assert!(
            !is_admitted_turtle_prefix(prefix),
            "{prefix} must fail the admission predicate"
        );
        assert!(
            !is_refused_private_prefix(prefix),
            "{prefix} is unknown, not a listed private token"
        );
    }
}

#[test]
fn shui_ui_property_group_and_editor_iris_are_open_admitted() {
    // Arrange — W3C SHACL 1.2 UI terms used for property groups + editors
    let parent_group = shui::PARENT_GROUP.as_str();
    let editor = shui::EDITOR.as_str();
    let editor_prop = shui::EDITOR_PROP.as_str();
    let viewer_prop = shui::VIEWER_PROP.as_str();
    let widget = shui::WIDGET.as_str();
    let private_lookalike = "http://example.org/truex/Widget";
    let shacl_core = "http://www.w3.org/ns/shacl#NodeShape";

    // Act / Assert — constants bind under the slash namespace
    assert_eq!(parent_group, "http://www.w3.org/ns/shacl-ui/parentGroup");
    assert_eq!(editor, "http://www.w3.org/ns/shacl-ui/Editor");
    assert_eq!(editor_prop, "http://www.w3.org/ns/shacl-ui/editor");
    assert_eq!(viewer_prop, "http://www.w3.org/ns/shacl-ui/viewer");
    assert_eq!(widget, "http://www.w3.org/ns/shacl-ui/Widget");

    // Act / Assert — open-admitted public namespace (Truex / gall fence must not refuse)
    for iri in [parent_group, editor, editor_prop, viewer_prop, widget] {
        assert!(
            is_open_admitted_namespace_iri(iri),
            "{iri} must be open-admitted under shui:"
        );
        assert!(shui::is_shui_iri(iri), "{iri} must match is_shui_iri");
    }

    assert!(!is_open_admitted_namespace_iri(private_lookalike));
    assert!(!shui::is_shui_iri(shacl_core));
    assert!(!shui::is_shui_iri(private_lookalike));
}
