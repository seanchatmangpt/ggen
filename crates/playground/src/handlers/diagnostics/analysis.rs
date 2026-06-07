use crate::handlers::completions::METHODS;
use crate::handlers::diagnostics::levenshtein_distance;
use syn::spanned::Spanned;

pub struct OverriddenMethod {
    pub name: String,
    pub span: proc_macro2::Span,
}

pub struct InvalidRpcName {
    pub name: String,
    pub span: proc_macro2::Span,
    pub message: String,
}

pub struct DeclaredCapability {
    pub name: String,
    pub span: proc_macro2::Span,
}

#[derive(Default)]
pub struct ImplAnalysis {
    pub overridden_methods: Vec<OverriddenMethod>,
    pub declared_capabilities: Vec<DeclaredCapability>,
    pub invalid_rpc_names: Vec<InvalidRpcName>,
    pub has_initialize: bool,
    pub has_shutdown: bool,
    pub has_initialized: bool,
    pub impl_trait_span: Option<proc_macro2::Span>,
    pub impl_close_brace_span: Option<proc_macro2::Span>,
    pub server_capabilities_open_brace_span: Option<proc_macro2::Span>,
    pub workspace_open_brace_span: Option<proc_macro2::Span>,
}

struct WorkspaceVisitor<'a> {
    declared_capabilities: &'a mut Vec<DeclaredCapability>,
    workspace_open_brace_span: &'a mut Option<proc_macro2::Span>,
}

impl<'ast, 'a> syn::visit::Visit<'ast> for WorkspaceVisitor<'a> {
    fn visit_expr_struct(&mut self, node: &'ast syn::ExprStruct) {
        println!(
            "DEBUG: WorkspaceVisitor visiting struct literal: {:?}",
            node.path.segments.last().map(|s| s.ident.to_string())
        );
        if self.workspace_open_brace_span.is_none() {
            *self.workspace_open_brace_span = Some(node.brace_token.span.open());
        }
        for field in &node.fields {
            if let syn::Member::Named(ident) = &field.member {
                let field_name = ident.to_string();
                println!("DEBUG: WorkspaceVisitor field_name: {}", field_name);
                if !is_none_expr(&field.expr) {
                    if field_name == "workspace_folders" {
                        self.declared_capabilities.push(DeclaredCapability {
                            name: "workspace.workspaceFolders".to_string(),
                            span: ident.span(),
                        });
                    } else if field_name == "file_operations" {
                        self.declared_capabilities.push(DeclaredCapability {
                            name: "workspace.fileOperations".to_string(),
                            span: ident.span(),
                        });
                    }
                }
            }
        }
        syn::visit::visit_expr_struct(self, node);
    }
}

pub fn analyze_impl_block(ast: &syn::File) -> ImplAnalysis {
    use syn::visit::Visit;

    struct Visitor {
        analysis: ImplAnalysis,
        in_ls_impl: bool,
        in_initialize_fn: bool,
    }

    impl<'ast> Visit<'ast> for Visitor {
        fn visit_item_trait(&mut self, node: &'ast syn::ItemTrait) {
            let has_rpc_attr = node
                .attrs
                .iter()
                .any(|attr| attr.meta.path().is_ident("rpc"));
            if has_rpc_attr {
                for item in &node.items {
                    if let syn::TraitItem::Fn(method) = item {
                        if let Some(attr) = method
                            .attrs
                            .iter()
                            .find(|attr| attr.meta.path().is_ident("rpc"))
                        {
                            let mut rpc_name = None;
                            let mut rpc_name_span = None;
                            let _ = attr.parse_nested_meta(|meta| {
                                if meta.path.is_ident("name") {
                                    let lit_str: syn::LitStr = meta.value()?.parse()?;
                                    rpc_name = Some(lit_str.value());
                                    rpc_name_span = Some(lit_str.span());
                                }
                                Ok(())
                            });
                            if let (Some(name), Some(span)) = (rpc_name, rpc_name_span) {
                                let is_valid = METHODS.iter().any(|m| m.lsp_method == name);
                                if !is_valid {
                                    let mut min_dist = usize::MAX;
                                    let mut closest_match: Option<&'static str> = None;
                                    for entry in METHODS {
                                        let dist = levenshtein_distance(&name, entry.lsp_method);
                                        if dist < min_dist {
                                            min_dist = dist;
                                            closest_match = Some(entry.lsp_method);
                                        }
                                    }
                                    let msg = if let Some(closest) = closest_match {
                                        if min_dist <= 4 {
                                            format!(
                                                "`{}` is not a valid RPC method. Did you mean '{}'?",
                                                name, closest
                                            )
                                        } else {
                                            format!("`{}` is not a valid RPC method.", name)
                                        }
                                    } else {
                                        format!("`{}` is not a valid RPC method.", name)
                                    };
                                    self.analysis.invalid_rpc_names.push(InvalidRpcName {
                                        name,
                                        span,
                                        message: msg,
                                    });
                                }
                            }
                        }
                    }
                }
            }
            syn::visit::visit_item_trait(self, node);
        }

        fn visit_item_impl(&mut self, node: &'ast syn::ItemImpl) {
            let is_ls_impl = node
                .trait_
                .as_ref()
                .and_then(|(_, path, _)| path.segments.last())
                .map(|s| s.ident == "LanguageServer")
                .unwrap_or(false);

            if is_ls_impl {
                self.in_ls_impl = true;
                if let Some((_, path, _)) = &node.trait_ {
                    self.analysis.impl_trait_span = Some(path.span());
                }
                self.analysis.impl_close_brace_span = Some(node.brace_token.span.close());
                syn::visit::visit_item_impl(self, node);
                self.in_ls_impl = false;
            } else {
                syn::visit::visit_item_impl(self, node);
            }
        }

        fn visit_impl_item_fn(&mut self, node: &'ast syn::ImplItemFn) {
            if !self.in_ls_impl {
                syn::visit::visit_impl_item_fn(self, node);
                return;
            }
            let name = node.sig.ident.to_string();
            self.analysis.overridden_methods.push(OverriddenMethod {
                name: name.clone(),
                span: node.sig.ident.span(),
            });
            match name.as_str() {
                "initialize" => self.analysis.has_initialize = true,
                "shutdown" => self.analysis.has_shutdown = true,
                "initialized" => self.analysis.has_initialized = true,
                _ => {}
            }
            if name == "initialize" {
                self.in_initialize_fn = true;
                syn::visit::visit_impl_item_fn(self, node);
                self.in_initialize_fn = false;
            } else {
                syn::visit::visit_impl_item_fn(self, node);
            }
        }

        fn visit_expr_struct(&mut self, node: &'ast syn::ExprStruct) {
            if !self.in_initialize_fn {
                syn::visit::visit_expr_struct(self, node);
                return;
            }
            let is_sc = node
                .path
                .segments
                .last()
                .map(|s| s.ident == "ServerCapabilities")
                .unwrap_or(false);
            if is_sc {
                self.analysis.server_capabilities_open_brace_span =
                    Some(node.brace_token.span.open());
                for field in &node.fields {
                    if let syn::Member::Named(ident) = &field.member {
                        let field_name = ident.to_string();
                        if !is_none_expr(&field.expr) {
                            if field_name == "workspace" {
                                let mut workspace_visitor = WorkspaceVisitor {
                                    declared_capabilities: &mut self.analysis.declared_capabilities,
                                    workspace_open_brace_span: &mut self
                                        .analysis
                                        .workspace_open_brace_span,
                                };
                                workspace_visitor.visit_expr(&field.expr);
                            } else {
                                self.analysis
                                    .declared_capabilities
                                    .push(DeclaredCapability {
                                        name: field_name,
                                        span: ident.span(),
                                    });
                            }
                        }
                    }
                }
            }
            syn::visit::visit_expr_struct(self, node);
        }
    }

    let mut v = Visitor {
        analysis: ImplAnalysis::default(),
        in_ls_impl: false,
        in_initialize_fn: false,
    };
    syn::visit::visit_file(&mut v, ast);
    v.analysis
}

fn is_none_expr(expr: &syn::Expr) -> bool {
    match expr {
        syn::Expr::Path(p) => p
            .path
            .segments
            .last()
            .map(|s| s.ident == "None")
            .unwrap_or(false),
        _ => false,
    }
}
