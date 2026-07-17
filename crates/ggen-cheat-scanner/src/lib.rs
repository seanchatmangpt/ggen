//! ggen-cheat-scanner: AST-based scanner for test-quality cheat patterns.
//!
//! Modeled on the pattern in `~/bcinr/tools/bcinr-cheat-scanner/src/main.rs`:
//! named rules with an ID, a `syn::visit::Visit`-based (or targeted-AST)
//! detector, and a required negative fixture proving the rule actually
//! catches a real example of the violation (see `tests/`).
//!
//! Four rules are implemented:
//!
//! - `CHEAT-T01` vacuous-assert
//! - `CHEAT-T02` tautological-result-check
//! - `CHEAT-T03` no-assertion-test
//! - `CHEAT-T04` mock-import
//!
//! `scan_source` runs the single-file rules (T01-T03, plus the unconditional
//! half of T04: `mockall` imports / `#[automock]`). The cross-file half of
//! T04 (a local `MockXxx`/`FakeXxx` struct standing in for a trait that real
//! production code also implements) requires aggregating impls across a
//! whole crate, so that part lives in [`collect_impls`] /
//! [`find_mock_substitutes`], run once per crate root by the binary (and
//! directly by tests).

use std::path::{Path, PathBuf};
use syn::visit::{self, Visit};
use syn::{Expr, ImplItemFn, ItemFn, ItemImpl, ItemUse};

/// A single rule violation found by the scanner.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Finding {
    pub rule_id: &'static str,
    pub file: PathBuf,
    pub line: usize,
    pub message: String,
}

impl std::fmt::Display for Finding {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "CHEAT[{}]: {}:{} — {}",
            self.rule_id,
            self.file.display(),
            self.line,
            self.message
        )
    }
}

/// Rule catalog entry (mirrors the `CheatRule` shape in
/// `bcinr-cheat-scanner`, trimmed to what this crate needs).
pub struct CheatRule {
    pub id: &'static str,
    pub title: &'static str,
    pub detection_contract: &'static str,
}

pub fn get_rules() -> Vec<CheatRule> {
    vec![
        CheatRule {
            id: "CHEAT-T01",
            title: "VACUOUS_ASSERT",
            detection_contract:
                "A #[test] function whose body is (or reduces to) assert!(true) as its only meaningful assertion.",
        },
        CheatRule {
            id: "CHEAT-T02",
            title: "TAUTOLOGICAL_RESULT_CHECK",
            detection_contract:
                "X.is_ok() || X.is_err() (or is_some()/is_none()) on the same scrutinee inside a #[test] function -- always true.",
        },
        CheatRule {
            id: "CHEAT-T03",
            title: "NO_ASSERTION_TEST",
            detection_contract:
                "A #[test] function with zero assert*!/.unwrap()/.expect()/panic-triggering calls anywhere in its body.",
        },
        CheatRule {
            id: "CHEAT-T04",
            title: "MOCK_IMPORT",
            detection_contract:
                "An import of mockall, a #[automock] attribute, or a local MockXxx/FakeXxx struct implementing a trait also implemented by real production code in the same crate.",
        },
    ]
}

fn has_attr_named(attrs: &[syn::Attribute], want: &str) -> bool {
    attrs.iter().any(|attr| {
        let s = quote::quote!(#attr).to_string();
        s.contains(want)
    })
}

fn is_test_fn(attrs: &[syn::Attribute]) -> bool {
    // #[test] or #[tokio::test]/#[async_std::test] etc -- anything ending in
    // `test` as the attribute path's last segment, plus the bare `#[test]`.
    attrs.iter().any(|attr| {
        let path = attr.path();
        path.segments.last().is_some_and(|seg| seg.ident == "test")
    })
}

/// Macro name (last path segment) for any `syn::Macro`, be it invoked as a
/// statement (`assert!(true);`), an expression (`let x = assert!(...)`,
/// rare), or anywhere else `syn::visit`'s `visit_macro` fires.
fn macro_name_of(m: &syn::Macro) -> String {
    m.path
        .segments
        .last()
        .map(|s| s.ident.to_string())
        .unwrap_or_default()
}

/// True if `m` is `assert!(true)` (allowing an optional trailing message
/// arg, which does not change the vacuousness of the check).
fn is_assert_true(m: &syn::Macro) -> bool {
    if macro_name_of(m) != "assert" {
        return false;
    }
    let tokens = m.tokens.to_string().replace(' ', "");
    // First token-stream argument is the literal `true` (either the whole
    // thing, or `true,"msg"`).
    tokens == "true" || tokens.starts_with("true,")
}

/// Collect every top-level `assert!`/`assert_eq!`/`assert_ne!`,
/// `.unwrap()`/`.expect()`/`.unwrap_err()`/`.expect_err()`, and
/// `panic!`/`unreachable!` call reachable inside a function body.
///
/// Overrides `visit_macro` (not `visit_expr`'s `Expr::Macro` arm) because
/// `assert!(true);` written as a bare statement parses as
/// `Stmt::Macro(StmtMacro)`, which `syn::visit`'s generated code reaches via
/// `visit_macro`, not `visit_expr` -- the far more common shape for a test
/// body than a macro used in expression position.
struct AssertionCollector {
    /// Every assertion-macro call found, in source order.
    asserts: Vec<syn::Macro>,
    /// True if any fallible/failure-triggering call was found at all
    /// (asserts, unwrap/expect family, panic/unreachable).
    any_failure_capable: bool,
}

impl<'ast> Visit<'ast> for AssertionCollector {
    fn visit_macro(&mut self, m: &'ast syn::Macro) {
        let name = macro_name_of(m);
        if matches!(name.as_str(), "assert" | "assert_eq" | "assert_ne") {
            self.asserts.push(m.clone());
            self.any_failure_capable = true;
        } else if matches!(name.as_str(), "panic" | "unreachable" | "todo" | "unimplemented") {
            self.any_failure_capable = true;
        }
        visit::visit_macro(self, m);
    }

    fn visit_expr(&mut self, e: &'ast Expr) {
        if let Expr::MethodCall(mc) = e {
            let m = mc.method.to_string();
            if matches!(m.as_str(), "unwrap" | "expect" | "unwrap_err" | "expect_err") {
                self.any_failure_capable = true;
            }
        }
        visit::visit_expr(self, e);
    }
}

fn collect_assertions(block: &syn::Block) -> AssertionCollector {
    let mut c = AssertionCollector {
        asserts: Vec::new(),
        any_failure_capable: false,
    };
    for stmt in &block.stmts {
        c.visit_stmt(stmt);
    }
    c
}

/// T02 detector: `X.is_ok() || X.is_err()` (or the Option equivalent) on the
/// same scrutinee.
struct TautologyVisitor<'a> {
    file: &'a Path,
    findings: Vec<Finding>,
}

const TAUTOLOGY_PAIRS: &[(&str, &str)] = &[("is_ok", "is_err"), ("is_some", "is_none")];

fn method_receiver_str(e: &Expr) -> Option<(String, String)> {
    if let Expr::MethodCall(mc) = e {
        let recv = &mc.receiver;
        let recv_str = quote::quote!(#recv).to_string().replace(' ', "");
        return Some((recv_str, mc.method.to_string()));
    }
    None
}

impl<'ast> Visit<'ast> for TautologyVisitor<'_> {
    fn visit_expr(&mut self, e: &'ast Expr) {
        if let Expr::Binary(b) = e {
            if matches!(b.op, syn::BinOp::Or(_)) {
                if let (Some((lr, lm)), Some((rr, rm))) =
                    (method_receiver_str(&b.left), method_receiver_str(&b.right))
                {
                    if lr == rr {
                        for (a, bmeth) in TAUTOLOGY_PAIRS {
                            if (lm == *a && rm == *bmeth) || (lm == *bmeth && rm == *a) {
                                self.findings.push(Finding {
                                    rule_id: "CHEAT-T02",
                                    file: self.file.to_path_buf(),
                                    line: line_of(e),
                                    message: format!(
                                        "tautological check `{lr}.{lm}() || {rr}.{rm}()` is always true"
                                    ),
                                });
                            }
                        }
                    }
                }
            }
        }
        visit::visit_expr(self, e);
    }
}

fn line_of(e: &Expr) -> usize {
    // syn's proc-macro2 spans carry line info when parsed from source text
    // via `proc_macro2::Span` (not the dummy call-site span produced by
    // `quote!`). `syn::parse_file` on real source text gives real spans.
    use syn::spanned::Spanned;
    e.span().start().line
}

fn item_fn_line(i: &ItemFn) -> usize {
    i.sig.ident.span().start().line
}

fn impl_item_fn_line(i: &ImplItemFn) -> usize {
    i.sig.ident.span().start().line
}

/// Top-level visitor: walks every `#[test]` fn (free fn or impl-block fn)
/// and applies T01/T02/T03; also flags T04's unconditional half (mockall
/// import / `#[automock]`).
struct ScanVisitor<'a> {
    file: &'a Path,
    findings: Vec<Finding>,
}

fn check_test_fn_body(
    fn_name: &str,
    line: usize,
    block: &syn::Block,
    file: &Path,
    findings: &mut Vec<Finding>,
) {
    let collector = collect_assertions(block);

    // T01: vacuous-assert -- the only assertion-macro call in the body is
    // `assert!(true)`.
    if collector.asserts.len() == 1 && is_assert_true(&collector.asserts[0]) {
        findings.push(Finding {
            rule_id: "CHEAT-T01",
            file: file.to_path_buf(),
            line,
            message: format!("test `{fn_name}` reduces to assert!(true) -- no real check"),
        });
    }

    // T03: no-assertion-test -- nothing in the body could ever fail.
    if !collector.any_failure_capable {
        findings.push(Finding {
            rule_id: "CHEAT-T03",
            file: file.to_path_buf(),
            line,
            message: format!(
                "test `{fn_name}` contains no assert*!/.unwrap()/.expect()/panic call -- it can never fail"
            ),
        });
    }

    // T02: tautological-result-check.
    let mut tv = TautologyVisitor {
        file,
        findings: Vec::new(),
    };
    tv.visit_block(block);
    findings.extend(tv.findings);
}

impl<'ast> Visit<'ast> for ScanVisitor<'_> {
    fn visit_item_fn(&mut self, i: &'ast ItemFn) {
        if is_test_fn(&i.attrs) {
            check_test_fn_body(
                &i.sig.ident.to_string(),
                item_fn_line(i),
                &i.block,
                self.file,
                &mut self.findings,
            );
        }
        visit::visit_item_fn(self, i);
    }

    fn visit_impl_item_fn(&mut self, i: &'ast ImplItemFn) {
        if is_test_fn(&i.attrs) {
            check_test_fn_body(
                &i.sig.ident.to_string(),
                impl_item_fn_line(i),
                &i.block,
                self.file,
                &mut self.findings,
            );
        }
        visit::visit_impl_item_fn(self, i);
    }

    fn visit_item_use(&mut self, i: &'ast ItemUse) {
        // T04 (unconditional half): `use mockall::...` or `use mockall;`
        let use_str = quote::quote!(#i).to_string();
        if use_str.contains("mockall") {
            use syn::spanned::Spanned;
            self.findings.push(Finding {
                rule_id: "CHEAT-T04",
                file: self.file.to_path_buf(),
                line: i.span().start().line,
                message: "import of mockall -- forbidden London-TDD mocking library (Chicago TDD only)".to_string(),
            });
        }
        visit::visit_item_use(self, i);
    }
}

// Separate pass for #[automock] since it can appear on a trait item, not
// just a fn -- walk every attribute-bearing item type we care about.
fn scan_automock(file: &Path, syntax: &syn::File, findings: &mut Vec<Finding>) {
    for item in &syntax.items {
        if let syn::Item::Trait(t) = item {
            if has_attr_named(&t.attrs, "automock") {
                use syn::spanned::Spanned;
                findings.push(Finding {
                    rule_id: "CHEAT-T04",
                    file: file.to_path_buf(),
                    line: t.span().start().line,
                    message: format!("#[automock] on trait `{}` -- forbidden London-TDD mocking (Chicago TDD only)", t.ident),
                });
            }
        }
    }
}

/// Run the single-file rules (T01, T02, T03, and the unconditional half of
/// T04) against one already-read source string. `path` is used only for
/// finding locations, it does not need to exist on disk.
pub fn scan_source(src: &str, path: &Path) -> Vec<Finding> {
    let mut findings = Vec::new();
    let Ok(syntax) = syn::parse_file(src) else {
        return findings;
    };
    let mut v = ScanVisitor {
        file: path,
        findings: Vec::new(),
    };
    v.visit_file(&syntax);
    findings.extend(v.findings);
    scan_automock(path, &syntax, &mut findings);
    findings
}

/// A `(trait_name, type_name, file, line)` impl record, collected across a
/// whole crate so T04's cross-file mock-substitute check can run.
#[derive(Debug, Clone)]
pub struct ImplRecord {
    pub trait_name: String,
    pub type_name: String,
    pub file: PathBuf,
    pub line: usize,
}

struct ImplCollector<'a> {
    file: &'a Path,
    records: Vec<ImplRecord>,
}

fn type_name_of(ty: &syn::Type) -> Option<String> {
    if let syn::Type::Path(tp) = ty {
        tp.path.segments.last().map(|s| s.ident.to_string())
    } else {
        None
    }
}

impl<'ast> Visit<'ast> for ImplCollector<'_> {
    fn visit_item_impl(&mut self, i: &'ast ItemImpl) {
        if let Some((_, trait_path, _)) = &i.trait_ {
            if let Some(trait_name) = trait_path.segments.last().map(|s| s.ident.to_string()) {
                if let Some(type_name) = type_name_of(&i.self_ty) {
                    use syn::spanned::Spanned;
                    self.records.push(ImplRecord {
                        trait_name,
                        type_name,
                        file: self.file.to_path_buf(),
                        line: i.span().start().line,
                    });
                }
            }
        }
        visit::visit_item_impl(self, i);
    }
}

/// Collect all `impl Trait for Type` records in one source file, for later
/// cross-file aggregation by [`find_mock_substitutes`].
pub fn collect_impls(src: &str, path: &Path) -> Vec<ImplRecord> {
    let Ok(syntax) = syn::parse_file(src) else {
        return Vec::new();
    };
    let mut c = ImplCollector {
        file: path,
        records: Vec::new(),
    };
    c.visit_file(&syntax);
    c.records
}

fn is_mock_or_fake_name(name: &str) -> bool {
    name.starts_with("Mock") || name.starts_with("Fake")
}

/// T04 (cross-file half): given every impl record collected across a crate,
/// flag a `MockXxx`/`FakeXxx` type that implements a trait ALSO implemented
/// by a differently-named (real production) type in the same crate.
///
/// This deliberately does not flag a `MockXxx`/`FakeXxx` type whose trait has
/// no other implementer (e.g. a pure trait-shape stub with nothing to
/// substitute for), nor a real testcontainer merely named `MockApiContainer`
/// with no trait collision -- both are false-positive shapes named in the
/// task brief.
pub fn find_mock_substitutes(records: &[ImplRecord]) -> Vec<Finding> {
    use std::collections::BTreeMap;
    let mut by_trait: BTreeMap<&str, Vec<&ImplRecord>> = BTreeMap::new();
    for r in records {
        by_trait.entry(r.trait_name.as_str()).or_default().push(r);
    }

    let mut findings = Vec::new();
    for (trait_name, impls) in by_trait {
        let mock_impls: Vec<&&ImplRecord> = impls
            .iter()
            .filter(|r| is_mock_or_fake_name(&r.type_name))
            .collect();
        if mock_impls.is_empty() {
            continue;
        }
        let real_impls: Vec<&&ImplRecord> = impls
            .iter()
            .filter(|r| !is_mock_or_fake_name(&r.type_name))
            .collect();
        if real_impls.is_empty() {
            continue;
        }
        for mock in mock_impls {
            findings.push(Finding {
                rule_id: "CHEAT-T04",
                file: mock.file.clone(),
                line: mock.line,
                message: format!(
                    "`{}` implements trait `{trait_name}`, which real production type `{}` also implements -- looks like a mock collaborator substitute",
                    mock.type_name, real_impls[0].type_name
                ),
            });
        }
    }
    findings
}

/// Should this path be skipped entirely (archive dirs, the scanner's own
/// fixture files)?
pub fn should_skip(path: &Path) -> bool {
    let s = path.to_string_lossy();
    s.contains("/archive/")
        || s.contains("/target/")
        || s.contains("ggen-cheat-scanner/tests/fixtures/")
}
