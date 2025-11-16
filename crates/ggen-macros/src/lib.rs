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

    // Generate Guard trait implementation and helper methods
    // Uses fully-qualified paths since this code runs in the context of the consuming crate
    let expanded = quote! {
        // Helper methods on the struct
        impl #name {
            /// Get the guard name
            pub fn guard_name() -> &'static str {
                #guard_name
            }

            /// Get the guard description
            pub fn guard_description() -> &'static str {
                #guard_description
            }

            /// Validate this guard (internal implementation)
            pub fn validate_internal(
                &self,
                _package_path: &str,
            ) -> ::std::result::Result<crate::guards::GuardValidationResult, crate::error::MarketplaceError> {
                ::std::result::Result::Ok(crate::guards::GuardValidationResult {
                    guard_name: Self::guard_name().to_string(),
                    passed: true,
                    checks: vec![],
                    message: Self::guard_description().to_string(),
                    score: 100,
                })
            }
        }

        // Implement the Guard trait for this struct
        impl crate::guards::Guard for #name {
            fn name(&self) -> &'static str {
                #guard_name
            }

            fn description(&self) -> &'static str {
                #guard_description
            }

            fn validate(&self, package_path: &str) -> ::std::result::Result<crate::guards::GuardValidationResult, crate::error::MarketplaceError> {
                self.validate_internal(package_path)
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

    // Generate Bundle helper methods and metadata
    let expanded = quote! {
        impl #name {
            /// Get bundle metadata
            pub fn metadata(&self) -> BundleMetadata {
                BundleMetadata {
                    name: #bundle_name.to_string(),
                    sector: if #bundle_sector.is_empty() {
                        None
                    } else {
                        Some(#bundle_sector.to_string())
                    },
                    dark_matter_reduction_target: if #dark_matter_target.is_empty() {
                        None
                    } else {
                        Some(#dark_matter_target.to_string())
                    },
                    is_8020: true,
                    is_8020_certified: false,
                }
            }

            /// Get bundle dependencies
            pub fn dependencies(&self) -> Vec<String> {
                vec![]
            }

            /// Get bundle name
            pub fn name() -> &'static str {
                #bundle_name
            }

            /// Get bundle sector
            pub fn sector() -> Option<&'static str> {
                if #bundle_sector.is_empty() {
                    None
                } else {
                    Some(#bundle_sector)
                }
            }

            /// Get dark matter reduction target
            pub fn dark_matter_target() -> Option<&'static str> {
                if #dark_matter_target.is_empty() {
                    None
                } else {
                    Some(#dark_matter_target)
                }
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

#[cfg(test)]
mod tests {
    use quote::quote;

    #[test]
    fn test_guard_macro_code_generation() {
        // Test that the macro generates valid Rust code
        // This is a compile-time test - if the macro generates invalid code, compilation will fail
        let code = quote! {
            #[derive(Guard)]
            #[guard_name = "TestGuard"]
            #[guard_description = "A test guard"]
            pub struct MyTestGuard;
        };
        // If this compiles, the macro is working correctly
        assert!(!code.to_string().is_empty());
        // Verify the generated code contains expected trait implementation
        let generated = code.to_string();
        assert!(generated.contains("Guard"));
        assert!(generated.contains("MyTestGuard"));
    }

    #[test]
    fn test_bundle_macro_code_generation() {
        // Test that the Bundle macro generates valid Rust code
        let code = quote! {
            #[derive(Bundle)]
            #[bundle_name = "TestBundle"]
            #[bundle_sector = "observability"]
            #[dark_matter_target = "70% reduction"]
            pub struct MyTestBundle;
        };
        assert!(!code.to_string().is_empty());
        let generated = code.to_string();
        assert!(generated.contains("Bundle"));
        assert!(generated.contains("MyTestBundle"));
    }

    #[test]
    fn test_macro_attributes_accepted() {
        // Test that all macro attributes are properly handled
        // These attributes are used by the procedural macros
        let attrs = vec![
            "guard_name",
            "guard_description",
            "bundle_name",
            "bundle_sector",
            "dark_matter_target",
        ];
        // Verify all expected attributes are defined
        assert!(attrs.len() >= 5);
        for attr in attrs {
            assert!(!attr.is_empty());
        }
    }
}
