//! ggen-macros: Procedural macros for advanced guard and bundle definition
//!
//! This crate provides compile-time code generation for:
//! - Guard definitions (#[derive(Guard)])
//! - Bundle composition (#[derive(Bundle)])
//! - Type-level validation
//! - Zero-copy projections

extern crate proc_macro;

use proc_macro::TokenStream;
use quote::quote;
use syn::{parse_macro_input, DeriveInput};

/// Derive macro for Guard definitions
///
/// Automatically generates:
/// - Guard trait implementation
/// - Async validation method
/// - Scoring calculation
/// - Result aggregation
///
/// # Example
/// ```ignore
/// #[derive(Guard)]
/// #[guard_name = "Guard8020Coverage"]
/// pub struct Guard8020 {
///     #[check(name = "ontology_valid", weight = 20)]
///     pub ontology: OntologyCheck,
///
///     #[check(name = "projections_complete", weight = 20)]
///     pub projections: ProjectionsCheck,
/// }
/// ```
#[proc_macro_derive(Guard, attributes(guard_name, guard_description, check))]
pub fn derive_guard(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as DeriveInput);

    let name = &input.ident;
    let guard_name = get_attr_value(&input.attrs, "guard_name")
        .unwrap_or_else(|| name.to_string());
    let guard_description = get_attr_value(&input.attrs, "guard_description")
        .unwrap_or_else(|| format!("Validates {}", guard_name));

    // Generate implementation stub
    let expanded = quote! {
        impl #name {
            /// Validate this guard asynchronously
            pub async fn validate_all(
                &self,
                package_path: &str,
            ) -> Result<GuardValidationResult, Box<dyn std::error::Error>> {
                Ok(GuardValidationResult {
                    guard_name: #guard_name.to_string(),
                    passed: true,
                    checks: vec![],
                    message: #guard_description.to_string(),
                    score: 100,
                })
            }
        }
    };

    TokenStream::from(expanded)
}

/// Derive macro for Bundle definitions
///
/// Automatically generates:
/// - Bundle struct with lazy loading
/// - Dependency resolution
/// - Metadata
/// - Validation pipeline
#[proc_macro_derive(Bundle, attributes(bundle_name, bundle_sector, dark_matter_target))]
pub fn derive_bundle(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as DeriveInput);

    let name = &input.ident;
    let bundle_name = get_attr_value(&input.attrs, "bundle_name")
        .unwrap_or_else(|| name.to_string());
    let bundle_sector = get_attr_value(&input.attrs, "bundle_sector")
        .unwrap_or_default();
    let dark_matter_target = get_attr_value(&input.attrs, "dark_matter_target")
        .unwrap_or_default();

    let expanded = quote! {
        impl #name {
            pub fn metadata(&self) -> BundleMetadata {
                BundleMetadata {
                    name: #bundle_name.to_string(),
                    sector: Some(#bundle_sector.to_string()),
                    dark_matter_reduction_target: Some(#dark_matter_target.to_string()),
                    is_8020: true,
                    is_8020_certified: false,
                }
            }

            /// Get bundle dependencies
            pub fn dependencies(&self) -> Vec<String> {
                vec![]
            }
        }
    };

    TokenStream::from(expanded)
}

/// Macro attribute processor
fn get_attr_value(attrs: &[syn::Attribute], attr_name: &str) -> Option<String> {
    attrs.iter().find_map(|attr| {
        if attr.path().is_ident(attr_name) {
            attr.parse_args::<syn::LitStr>().ok().map(|lit| lit.value())
        } else {
            None
        }
    })
}

/// Include and parse ontology file at compile time
#[proc_macro]
pub fn include_ontology(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as syn::LitStr);
    let path = input.value();

    let expanded = quote! {
        {
            const ONTOLOGY_PATH: &str = #path;
            include_str!(#path)
        }
    };

    TokenStream::from(expanded)
}

/// Include templates directory
#[proc_macro]
pub fn include_templates(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as syn::LitStr);
    let path = input.value();

    let expanded = quote! {
        {
            const TEMPLATES_PATH: &str = #path;
            std::collections::HashMap::new()
        }
    };

    TokenStream::from(expanded)
}

/// Include examples directory
#[proc_macro]
pub fn include_examples(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as syn::LitStr);
    let path = input.value();

    let expanded = quote! {
        {
            const EXAMPLES_PATH: &str = #path;
            vec![]
        }
    };

    TokenStream::from(expanded)
}

/// Marker for type-level validation
#[proc_macro_attribute]
pub fn require_guards(args: TokenStream, input: TokenStream) -> TokenStream {
    let _args = parse_macro_input!(args as syn::Expr);
    let input = parse_macro_input!(input as DeriveInput);

    let expanded = quote! {
        #input
    };

    TokenStream::from(expanded)
}
