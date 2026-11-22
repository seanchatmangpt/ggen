//! W3C SHACL 1.2 User Interfaces (`shui:`) vocabulary constants.
//!
//! Namespace per <https://www.w3.org/TR/shacl12-ui/> (Working Draft):
//! `http://www.w3.org/ns/shacl-ui/` (slash IRI, not hash).
//!
//! Gate 1 foundation for ECOSYSTEM-SPEC-27.1.1: admit `shui:` as a *public*
//! W3C vocabulary so ggen private-namespace fences do not refuse it. This
//! module does not implement Truex product admission (sibling repo).
//!
//! UI property groups nest via `shui:parentGroup` (value is a `sh:PropertyGroup`);
//! property shapes pick editors/viewers via `shui:editor` / `shui:viewer`.

use oxigraph::model::NamedNodeRef;

/// Turtle / compact prefix name.
pub const PREFIX: &str = "shui";

/// SHACL UI namespace URI (slash form).
pub const NAMESPACE: &str = "http://www.w3.org/ns/shacl-ui/";

/// `shui:Widget`
pub const WIDGET: NamedNodeRef<'static> =
    NamedNodeRef::new_unchecked("http://www.w3.org/ns/shacl-ui/Widget");

/// `shui:Editor`
pub const EDITOR: NamedNodeRef<'static> =
    NamedNodeRef::new_unchecked("http://www.w3.org/ns/shacl-ui/Editor");

/// `shui:Viewer`
pub const VIEWER: NamedNodeRef<'static> =
    NamedNodeRef::new_unchecked("http://www.w3.org/ns/shacl-ui/Viewer");

/// `shui:Configuration`
pub const CONFIGURATION: NamedNodeRef<'static> =
    NamedNodeRef::new_unchecked("http://www.w3.org/ns/shacl-ui/Configuration");

/// `shui:widget`
pub const WIDGET_PROP: NamedNodeRef<'static> =
    NamedNodeRef::new_unchecked("http://www.w3.org/ns/shacl-ui/widget");

/// `shui:editor` — preferred editor on a property shape.
pub const EDITOR_PROP: NamedNodeRef<'static> =
    NamedNodeRef::new_unchecked("http://www.w3.org/ns/shacl-ui/editor");

/// `shui:viewer` — preferred viewer on a property shape.
pub const VIEWER_PROP: NamedNodeRef<'static> =
    NamedNodeRef::new_unchecked("http://www.w3.org/ns/shacl-ui/viewer");

/// `shui:parentGroup` — nests a `sh:PropertyGroup` inside another property group.
pub const PARENT_GROUP: NamedNodeRef<'static> =
    NamedNodeRef::new_unchecked("http://www.w3.org/ns/shacl-ui/parentGroup");

/// Returns true when `iri` is under the W3C SHACL UI namespace.
pub fn is_shui_iri(iri: &str) -> bool {
    iri.starts_with(NAMESPACE)
}
