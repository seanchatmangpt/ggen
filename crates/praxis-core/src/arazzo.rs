//! Arazzo Projection Receipt binding, plus (PROJ-752) the Rail A Tera
//! renderer T that manufactures an Arazzo 1.1.x document from PROJ-751's
//! real Q-stage SPARQL projection rows: `A_z = T(Q(W))` (PRD.md sec.7.4).

use crate::error::CoreError;
use powl2_decompose::Powl;
use praxis_graphlaw::chatman::powl_projection::{
    powl_to_turtle, run_render_model_projection, ExternalCutCompilationOutcome,
    ExternalCutCompilationRequest, ExternalCutCompiler, ProjectionRow,
    RENDER_MODEL_PROJECTION_QUERY,
};
use serde::{Deserialize, Serialize};
use wasm4pm_arazzo::compile::AirCompiler;
use wasm4pm_arazzo::normalizer::ArazzoNormalizer;
use wasm4pm_arazzo::parse::DocumentIndex;
use wasm4pm_arazzo::{lower, resolve};

/// Binding for the Arazzo Projection Receipt as required by PRD Iteration 8.
///
/// Binds:
/// - source POWL digest
/// - external-cut identity
/// - SPARQL projection digest
/// - Tera template digest
/// - Arazzo digest
/// - compiler version
/// - AIR digest
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct ArazzoProjectionReceipt {
    /// source POWL digest (hex)
    pub source_powl_digest_hex: String,
    /// external-cut identity
    pub external_cut_identity: String,
    /// SPARQL projection digest (hex)
    pub sparql_projection_digest_hex: String,
    /// Tera template digest (hex)
    pub tera_template_digest_hex: String,
    /// Arazzo digest (hex)
    pub arazzo_digest_hex: String,
    /// compiler version
    pub compiler_version: String,
    /// AIR digest (hex)
    pub air_digest_hex: String,
}

impl ArazzoProjectionReceipt {
    /// Compute the receipt's canonical BLAKE3 digest from its facts
    /// serialized to canonical N-Quads order.
    pub fn compute_digest(&self) -> Result<[u8; 32], CoreError> {
        // Construct canonical N-Quads representing these facts.
        let subject = format!(
            "<urn:praxis:arazzo:projection:{}>",
            self.external_cut_identity
        );
        let mut quads = [
            format!(
                "{subject} <urn:praxis:predicate:source_powl_digest> \"{}\" .",
                self.source_powl_digest_hex
            ),
            format!(
                "{subject} <urn:praxis:predicate:external_cut_identity> \"{}\" .",
                self.external_cut_identity
            ),
            format!(
                "{subject} <urn:praxis:predicate:sparql_projection_digest> \"{}\" .",
                self.sparql_projection_digest_hex
            ),
            format!(
                "{subject} <urn:praxis:predicate:tera_template_digest> \"{}\" .",
                self.tera_template_digest_hex
            ),
            format!(
                "{subject} <urn:praxis:predicate:arazzo_digest> \"{}\" .",
                self.arazzo_digest_hex
            ),
            format!(
                "{subject} <urn:praxis:predicate:compiler_version> \"{}\" .",
                self.compiler_version
            ),
            format!(
                "{subject} <urn:praxis:predicate:air_digest> \"{}\" .",
                self.air_digest_hex
            ),
        ];

        // "All facts in canonical N-Quads order" means lexicographically sorted.
        quads.sort();

        // Join with newlines and add a trailing newline (standard N-Quads).
        let nquads_str = format!("{}\n", quads.join("\n"));

        let digest = *blake3::hash(nquads_str.as_bytes()).as_bytes();
        Ok(digest)
    }

    /// Builds a receipt from the real materials of one Rail A projection
    /// run: the admitted POWL region's Turtle (`W`), the SPARQL projection
    /// text actually executed against it (`Q`), the Tera template text
    /// actually rendered (`T`), and the resulting Arazzo JSON document
    /// (`A_z = T(Q(W))`, PRD.md sec.7.4).
    ///
    /// `source_powl_digest_hex`, `sparql_projection_digest_hex`,
    /// `tera_template_digest_hex`, and `arazzo_digest_hex` are computed here
    /// via BLAKE3 over the real material bytes the caller supplies -- never
    /// hand-typed placeholder strings (the prior state this ticket closes:
    /// `arazzo_projection_receipt_digest_is_deterministic` below is the only
    /// other populated instance in the crate, and it is deliberately built
    /// from literal placeholder strings for a pure digest-stability test,
    /// not presented as a real projection run).
    ///
    /// `air_digest_hex` remains caller-supplied: Arazzo -> AIR lowering
    /// (PROJ-753) does not exist in this codebase yet, so there is no real
    /// AIR artifact to hash here. Callers ahead of PROJ-753 landing should
    /// pass an explicit not-yet-available sentinel (e.g.
    /// `"unavailable:PROJ-753"`) rather than fabricate a digest of nothing.
    ///
    /// # Complexity
    /// O(b) where b is the total byte length of the four hashed materials
    /// (one BLAKE3 pass per material).
    pub fn from_materials(
        source_powl_turtle: &str,
        external_cut_identity: &str,
        sparql_query_text: &str,
        tera_template_text: &str,
        arazzo_document_json: &str,
        compiler_version: &str,
        air_digest_hex: &str,
    ) -> Self {
        Self {
            source_powl_digest_hex: hex::encode(
                blake3::hash(source_powl_turtle.as_bytes()).as_bytes(),
            ),
            external_cut_identity: external_cut_identity.to_string(),
            sparql_projection_digest_hex: hex::encode(
                blake3::hash(sparql_query_text.as_bytes()).as_bytes(),
            ),
            tera_template_digest_hex: hex::encode(
                blake3::hash(tera_template_text.as_bytes()).as_bytes(),
            ),
            arazzo_digest_hex: hex::encode(
                blake3::hash(arazzo_document_json.as_bytes()).as_bytes(),
            ),
            compiler_version: compiler_version.to_string(),
            air_digest_hex: air_digest_hex.to_string(),
        }
    }
}

// ── Manufacture admission (PROJ-783) ────────────────────────────────────
//
// PRD.md v26.7.11 sec.7.5: "Production Arazzo without an admitted POWL
// source and projection receipt SHALL be refused." Acceptance scenario
// 19.3 ("Handwritten Arazzo Is Refused"): "Given production Arazzo without
// a projection receipt and admitted POWL source, execution SHALL return
// ARAZZO_UNMANUFACTURED." Everything from `render_and_compile` through
// `ChatmanRailAbCompiler` (below) is the *manufacture* side of Rail A/B;
// this function is the *admission* side -- the gate a presented Arazzo
// document plus its claimed receipt must pass before either is trusted as
// a genuine `A_z = T(Q(W))` artifact rather than handwritten production
// Arazzo wearing a receipt-shaped decoration.

/// Admits a presented Arazzo document as genuinely *manufactured* (Rail
/// A's `A_z = T(Q(W))`, PRD.md sec.7.4-7.5) rather than handwritten
/// production Arazzo (PRD.md sec.5's non-goal "accept arbitrary Arazzo as
/// production workflow authority").
///
/// Three independent, ordered checks, each a distinct PRD.md sec.18 typed
/// refusal:
/// 1. [`CoreError::ArazzoUnmanufactured`] (`ARAZZO_UNMANUFACTURED`) -- no
///    projection receipt was presented at all (acceptance scenario 19.3).
/// 2. [`CoreError::ArazzoSourceReceiptMissing`]
///    (`ARAZZO_SOURCE_RECEIPT_MISSING`) -- a receipt was presented, but it
///    does not actually bind to an admitted POWL source
///    (`source_powl_digest_hex`/`external_cut_identity` empty) -- a
///    receipt shape that attests nothing.
/// 3. [`CoreError::ArazzoProjectionDigestMismatch`]
///    (`ARAZZO_PROJECTION_DIGEST_MISMATCH`) -- the receipt's own
///    `arazzo_digest_hex` does not equal the real BLAKE3 digest of the
///    presented `arazzo_document` bytes -- the receipt was cut over
///    different material than what is actually being presented (a stale,
///    substituted, or hand-edited document wearing a real receipt).
///
/// # Complexity
/// O(b) where b is the byte length of `arazzo_document` (one BLAKE3 pass);
/// every other check is O(1) string comparison.
pub fn admit_manufactured_arazzo(
    arazzo_document: &str,
    receipt: Option<&ArazzoProjectionReceipt>,
) -> Result<(), CoreError> {
    let Some(receipt) = receipt else {
        return Err(CoreError::ArazzoUnmanufactured(format!(
            "no projection receipt presented for this Arazzo document ({} bytes); \
             production Arazzo without an admitted POWL source and projection receipt is \
             refused (PRD.md v26.7.11 sec.7.5)",
            arazzo_document.len()
        )));
    };
    if receipt.source_powl_digest_hex.is_empty() || receipt.external_cut_identity.is_empty() {
        return Err(CoreError::ArazzoSourceReceiptMissing(format!(
            "projection receipt does not bind an admitted POWL source \
             (source_powl_digest_hex={:?}, external_cut_identity={:?})",
            receipt.source_powl_digest_hex, receipt.external_cut_identity
        )));
    }
    let recomputed = hex::encode(blake3::hash(arazzo_document.as_bytes()).as_bytes());
    if recomputed != receipt.arazzo_digest_hex {
        return Err(CoreError::ArazzoProjectionDigestMismatch(format!(
            "presented Arazzo document recomputes to {recomputed}, but the projection \
             receipt carries {}; the receipt does not attest this document's bytes",
            receipt.arazzo_digest_hex
        )));
    }
    Ok(())
}

/// PROJ-777/778: admits a presented Arazzo document the same way as
/// [`admit_manufactured_arazzo`], but first requires the caller to declare
/// which GraphLaw dialect it is presenting under, and checks that
/// declaration against [`crate::graphlaw_authority::REGISTRY`] before
/// running the existing manufacture checks. This is the dialect-authority
/// gate PROJ-778 requires ("No dialect SHALL acquire authority from another
/// dialect merely because it can encode equivalent syntax",
/// `docs/jira/v26.7.11/PRD.md:588`) applied concretely to Arazzo manufacture
/// admission: a caller cannot obtain Arazzo's manufacture authority by
/// presenting a document under an unregistered dialect name, or under a
/// different *registered* dialect's name -- even one whose own authority
/// also covers graph consequence (e.g. `"SPARQL CONSTRUCT"`, authority
/// `"manufacture graph consequence"`) -- only a document declared exactly
/// `"Arazzo"` reaches the real admission checks in
/// [`admit_manufactured_arazzo`].
///
/// [`admit_manufactured_arazzo`] itself is unchanged and remains directly
/// callable (no breaking change to its existing signature or callers, per
/// this crate's API-stability rule); this is an additive, stricter entry
/// point for callers that also want the dialect-name check enforced.
///
/// # Complexity
/// O(D) for the registry lookup (D = 14 registered dialects, a fixed
/// constant -- a linear scan of a `&'static` slice, per
/// [`crate::graphlaw_authority::authority_for`]'s own doc), plus
/// [`admit_manufactured_arazzo`]'s O(b) BLAKE3 pass over the document bytes.
pub fn admit_manufactured_arazzo_for_dialect(
    declared_dialect: &str,
    arazzo_document: &str,
    receipt: Option<&ArazzoProjectionReceipt>,
) -> Result<(), CoreError> {
    let declaration =
        crate::graphlaw_authority::authority_for(declared_dialect).ok_or_else(|| {
            CoreError::ArazzoDialectAuthorityMismatch {
                declared: declared_dialect.to_string(),
                expected: "Arazzo",
                reason: "declared dialect is not registered in the GraphLaw authority registry"
                    .to_string(),
            }
        })?;
    if declaration.name != "Arazzo" {
        return Err(CoreError::ArazzoDialectAuthorityMismatch {
            declared: declared_dialect.to_string(),
            expected: "Arazzo",
            reason: format!(
                "declared dialect holds authority {:?}, not Arazzo's \"manufactured \
                 inter-engine workflow carrier\" authority",
                declaration.authority
            ),
        });
    }
    admit_manufactured_arazzo(arazzo_document, receipt)
}

// ── T stage: Tera renderer (PROJ-752) ───────────────────────────────────

/// The Rail A Arazzo-manufacture Tera template. Single source of truth is
/// the checked-in template file itself
/// (`crates/praxis-core/templates/arazzo_projection.tera`), embedded at
/// compile time so there is exactly one copy of the template text.
const ARAZZO_PROJECTION_TEMPLATE: &str = include_str!("../templates/arazzo_projection.tera");

/// One ordered step resolved from a [`ProjectionRow`] slice: either a leaf
/// activity (from a `powl2:ActivityLeaf`/`powl2:SilentLeaf`) or an external
/// cut boundary (from a `powl2:ExternalCut`), never both -- the source
/// `Powl` enum makes the two mutually exclusive per node.
#[derive(Debug, Clone, PartialEq, Eq)]
struct StepProjection {
    /// The projected element's subject IRI (becomes the manufactured
    /// step's `operationId`: the identifier of the POWL element this step
    /// was projected from, not a resolved external API operation).
    element_id: String,
    /// Human-readable step description: the activity label, or a
    /// synthetic label for a silent leaf / external cut.
    label: String,
    /// True when this step is an external-cut boundary rather than a
    /// leaf activity.
    is_external_cut: bool,
    /// `powl2:hasRegion` target, present only when `is_external_cut`.
    region_id: Option<String>,
    /// `powl2:sparqlProjection` literal (Q), present only when
    /// `is_external_cut`.
    sparql_projection: Option<String>,
    /// `powl2:teraRenderer` literal (T), present only when
    /// `is_external_cut`.
    tera_renderer: Option<String>,
}

/// Tera context row for one manufactured Arazzo step. `Serialize`d directly
/// into the [`ARAZZO_PROJECTION_TEMPLATE`] context under `steps`.
#[derive(Debug, Clone, Serialize)]
struct StepView {
    step_id: String,
    description: String,
    operation_id: String,
    is_external_cut: bool,
    region_id: Option<String>,
    sparql_projection: Option<String>,
    tera_renderer: Option<String>,
}

/// Resolves the ordered sequence of [`StepProjection`]s reachable from
/// `element_id` by walking `rows`' `childIndex`/`childModel` bindings
/// depth-first in ascending index order (a container's children), bottoming
/// out at a leaf, silent leaf, or external-cut row (a node with no bound
/// `childIndex`/`childModel` of its own).
///
/// An external cut's own region is *not* recursed into: the region is, by
/// definition of Rail A's external-cut boundary (PRD.md sec.7.4), opaque to
/// the projecting workflow -- it is dispatched to, not enumerated as this
/// workflow's own steps. That the region's rows are present in the wider
/// `rows` slice (any admitted subtree round-trips through
/// `powl_to_turtle`/`run_render_model_projection`) does not make them steps
/// of *this* manufactured document.
///
/// # Complexity
/// O(n * d) where n = `rows.len()` and d = the tree depth rooted at
/// `element_id`: each call does one O(n) linear scan of `rows` (no
/// `HashMap`, no incidental ordering reliance -- children are sorted
/// explicitly by their parsed numeric index) and recurses once per child.
/// Acceptable for the admitted-region sizes this pipeline handles (bounded
/// PDDL plan tapes, same bound documented on
/// `project_pddl_tape_to_powl`/`Pddl8Tape`); not a hidden quadratic on
/// unbounded input.
///
/// # Errors
/// [`CoreError::UnresolvedProjectionElement`] if `element_id` (or a
/// `childModel` it references) has no row at all in `rows`, or if a bound
/// `childIndex` literal is not a valid non-negative integer -- both
/// indicate `rows` did not actually come from a well-formed
/// `run_render_model_projection` run over `element_id`'s own admitted
/// region.
fn flatten_ordered_steps(
    rows: &[ProjectionRow],
    element_id: &str,
) -> Result<Vec<StepProjection>, CoreError> {
    let own_rows: Vec<&ProjectionRow> =
        rows.iter().filter(|r| r.element_id == element_id).collect();
    if own_rows.is_empty() {
        return Err(CoreError::UnresolvedProjectionElement(
            element_id.to_string(),
        ));
    }

    // Ordered children (containers only): rows binding childIndex+childModel
    // for this element_id, keyed by the *parsed* numeric index in a
    // `BTreeMap` -- sorted deterministically by key (never a `HashMap`), and
    // deduplicating the exact-duplicate (childIndex, childModel) rows a
    // multi-`rdf:type` subject produces. `emit_powl_node` gives the model
    // root two `rdf:type`s (`powl2:Model` and its own container type, e.g.
    // `powl2:PartialOrder`), and `?elementId a ?elementType` yields one row
    // per type per SPARQL BGP semantics -- so a 2-child root's `hasChild`
    // OPTIONAL is cross-joined into 4 raw rows (2 types x 2 children), not
    // 2. A genuine inconsistency (the same index bound to two *different*
    // models) is refused rather than silently resolved by picking one.
    let mut children: std::collections::BTreeMap<u64, String> = std::collections::BTreeMap::new();
    for row in &own_rows {
        if let (Some(idx_lexical), Some(model)) = (&row.child_index, &row.child_model) {
            let idx: u64 = idx_lexical.parse().map_err(|_| {
                CoreError::UnresolvedProjectionElement(format!(
                    "non-numeric childIndex {idx_lexical:?} on element {element_id}"
                ))
            })?;
            if let Some(existing) = children.get(&idx) {
                if existing != model {
                    return Err(CoreError::UnresolvedProjectionElement(format!(
                        "childIndex {idx} on element {element_id} bound to two \
                         different childModel values: {existing:?} and {model:?}"
                    )));
                }
            } else {
                children.insert(idx, model.clone());
            }
        }
    }

    if !children.is_empty() {
        let mut out = Vec::new();
        for (_, child_id) in children {
            out.extend(flatten_ordered_steps(rows, &child_id)?);
        }
        return Ok(out);
    }

    if let Some(cut_row) = own_rows
        .iter()
        .find(|r| r.element_type.ends_with("ExternalCut"))
    {
        return Ok(vec![StepProjection {
            element_id: element_id.to_string(),
            label: format!("external_cut:{element_id}"),
            is_external_cut: true,
            region_id: cut_row.region_id.clone(),
            sparql_projection: cut_row.sparql_projection.clone(),
            tera_renderer: cut_row.tera_renderer.clone(),
        }]);
    }

    if let Some(leaf_row) = own_rows.iter().find(|r| r.activity_label.is_some()) {
        return Ok(vec![StepProjection {
            element_id: element_id.to_string(),
            label: leaf_row
                .activity_label
                .clone()
                .expect("filtered on activity_label.is_some() immediately above"),
            is_external_cut: false,
            region_id: None,
            sparql_projection: None,
            tera_renderer: None,
        }]);
    }

    // No children, no ExternalCut row, no activityLabel: a silent leaf
    // (powl2:Leaf, powl2:SilentLeaf -- Powl::Leaf(None)).
    Ok(vec![StepProjection {
        element_id: element_id.to_string(),
        label: "silent".to_string(),
        is_external_cut: false,
        region_id: None,
        sparql_projection: None,
        tera_renderer: None,
    }])
}

/// The T stage of Rail A's `A_z = T(Q(W))` (PRD.md sec.7.4): renders
/// PROJ-751's real Q-stage SPARQL projection rows into a manufactured
/// Arazzo 1.1.x JSON document via [`ARAZZO_PROJECTION_TEMPLATE`].
///
/// `rows` is the output of
/// `praxis_graphlaw::chatman::powl_projection::run_render_model_projection`
/// over an admitted POWL region's Turtle; `root_element_id` is that same
/// region's root subject IRI (the `<base_iri>/<path>` the caller passed to
/// `powl_to_turtle`). The returned `String` is the rendered JSON text --
/// callers that need an `ArazzoDescription` parse it with
/// `wasm4pm_arazzo::parse::DocumentIndex::add_document` (see this module's
/// own round-trip test).
///
/// # Errors
/// [`CoreError::UnresolvedProjectionElement`] (see [`flatten_ordered_steps`])
/// or [`CoreError::TemplateRenderFailed`] if the projection resolves to zero
/// steps, or if Tera itself refuses the render (a crate defect in
/// [`ARAZZO_PROJECTION_TEMPLATE`], since every context value passed to it is
/// a plain string/bool routed through the `json_encode` filter).
///
/// # Complexity
/// O(n * d) for [`flatten_ordered_steps`] plus O(s) for the Tera render,
/// where s is the rendered document's byte length.
pub fn render_arazzo_document(
    rows: &[ProjectionRow],
    root_element_id: &str,
    workflow_id: &str,
    title: &str,
) -> Result<String, CoreError> {
    let steps = flatten_ordered_steps(rows, root_element_id)?;
    if steps.is_empty() {
        return Err(CoreError::TemplateRenderFailed(format!(
            "projection rooted at {root_element_id} yielded zero Arazzo steps"
        )));
    }

    let step_views: Vec<StepView> = steps
        .into_iter()
        .enumerate()
        .map(|(i, s)| StepView {
            step_id: format!("step_{i:03}"),
            description: s.label,
            operation_id: s.element_id,
            is_external_cut: s.is_external_cut,
            region_id: s.region_id,
            sparql_projection: s.sparql_projection,
            tera_renderer: s.tera_renderer,
        })
        .collect();

    let mut context = tera::Context::new();
    context.insert("title", title);
    context.insert("workflow_id", workflow_id);
    context.insert("source_url", &format!("{root_element_id}#source"));
    context.insert("steps", &step_views);

    tera::Tera::one_off(ARAZZO_PROJECTION_TEMPLATE, &context, false)
        .map_err(|e| CoreError::TemplateRenderFailed(format!("{e:?}")))
}

// ── Rail A/B production pipeline (PROJ-796) ─────────────────────────────
//
// Before this section, every stage from Q(W) SPARQL execution through AIR
// compilation was real and independently tested, but the full composition
// existed only inside this module's own `#[cfg(test)]` round-trip test
// (`manufactured_arazzo_round_trips_through_wasm4pm_arazzo_parser`, below).
// `ArazzoProjectionReceipt::project_and_compile` and `ChatmanRailAbCompiler`
// are the real, non-test-only entry points: the former for direct callers
// of this crate, the latter as the concrete implementation
// `ChatmanEngine::admit_transition_with_external_cut`
// (`praxis_graphlaw::chatman::engine`) invokes through the
// `ExternalCutCompiler` seam (see that trait's doc comment for why a trait
// seam is needed instead of a direct call: `praxis-graphlaw` cannot depend
// on this crate without a cyclic crate dependency).

/// Real, non-test-only output of [`ArazzoProjectionReceipt::
/// project_and_compile`]: the manufactured Arazzo document, the receipt
/// binding every material digest, and the compiled AIR module -- both its
/// digest (`receipt.air_digest_hex`, carried again here as raw bytes) and
/// its compiled WASM bytes, for callers that need to inspect the compiled
/// module without recompiling it.
#[derive(Debug, Clone, PartialEq)]
pub struct ArazzoCompilationArtifact {
    /// The receipt binding every material digest of this run.
    pub receipt: ArazzoProjectionReceipt,
    /// The manufactured Arazzo 1.1.x JSON document text (`A_z`).
    pub arazzo_document: String,
    /// The compiled AIR module's own digest
    /// (`wasm4pm_arazzo::compile::AirCompiler::digest_program`), matching
    /// `receipt.air_digest_hex`.
    pub air_digest: [u8; 32],
    /// The compiled AIR module's WASM bytes
    /// (`wasm4pm_arazzo::compile::AirCompiler::compile_to_wasm`).
    pub air_wasm: Vec<u8>,
}

/// Shared core of the Rail A/B pipeline (Q(W) SPARQL projection, T Tera
/// render, Arazzo parse/resolve, AIR lowering/normalization/compile):
/// called by both [`ArazzoProjectionReceipt::project_and_compile`] (which
/// additionally runs POWL admission/Turtle-emission first) and
/// [`ChatmanRailAbCompiler::compile`] (which receives an already-admitted
/// Turtle from `ChatmanEngine::admit_transition_with_external_cut`) -- one
/// real implementation, never two, so both entry points compute
/// byte-identical materials for the same inputs.
///
/// # Errors
/// [`CoreError::ExternalCutCompilationFailed`], naming the failing
/// sub-stage (`sparql_projection`, `arazzo_parse`, `uri_resolution`,
/// `air_lowering`, `air_normalization`, `air_compile`); or
/// [`CoreError::TemplateRenderFailed`] / [`CoreError::
/// UnresolvedProjectionElement`] from [`render_arazzo_document`].
///
/// # Complexity
/// O(n * d) for the Q(W) projection/step flattening (see
/// `flatten_ordered_steps`) plus O(s) for the Tera render plus the AIR
/// compiler's own bound (linear in program size).
fn render_and_compile(
    turtle: &str,
    root_element_id: &str,
    workflow_id: &str,
    title: &str,
    compiler_version: &str,
) -> Result<(ExternalCutCompilationOutcome, Vec<u8>), CoreError> {
    let rows = run_render_model_projection(turtle).map_err(|e| {
        CoreError::ExternalCutCompilationFailed {
            stage: "sparql_projection".to_string(),
            detail: e.to_string(),
        }
    })?;
    let arazzo_json = render_arazzo_document(&rows, root_element_id, workflow_id, title)?;

    // Synthetic document key, distinct from `root_element_id` itself (which
    // names the *source* POWL element, not the manufactured Arazzo
    // document) -- must be a valid absolute URI: `resolve::normalize_uris`
    // parses it with `url::Url::parse`.
    let doc_key = format!("{root_element_id}/manufactured");
    let mut index = DocumentIndex::new();
    index.add_document(&arazzo_json, &doc_key).map_err(|e| {
        CoreError::ExternalCutCompilationFailed {
            stage: "arazzo_parse".to_string(),
            detail: e.to_string(),
        }
    })?;
    resolve::normalize_uris(&mut index).map_err(|e| CoreError::ExternalCutCompilationFailed {
        stage: "uri_resolution".to_string(),
        detail: e.to_string(),
    })?;
    let parsed =
        index
            .documents
            .get(&doc_key)
            .ok_or_else(|| CoreError::ExternalCutCompilationFailed {
                stage: "uri_resolution".to_string(),
                detail: format!("document {doc_key:?} missing from the index after normalize_uris"),
            })?;

    let bump = bumpalo::Bump::new();
    let mut air_program = lower::lower_description(parsed, &bump).map_err(|e| {
        CoreError::ExternalCutCompilationFailed {
            stage: "air_lowering".to_string(),
            detail: e.to_string(),
        }
    })?;
    ArazzoNormalizer::normalize(&mut air_program, &bump).map_err(|e| {
        CoreError::ExternalCutCompilationFailed {
            stage: "air_normalization".to_string(),
            detail: e.to_string(),
        }
    })?;
    let air_wasm = AirCompiler::compile_to_wasm(&air_program).map_err(|e| {
        CoreError::ExternalCutCompilationFailed {
            stage: "air_compile".to_string(),
            detail: e.to_string(),
        }
    })?;
    let air_digest = AirCompiler::digest_program(&air_program).map_err(|e| {
        CoreError::ExternalCutCompilationFailed {
            stage: "air_compile".to_string(),
            detail: e.to_string(),
        }
    })?;

    let outcome = ExternalCutCompilationOutcome {
        source_powl_digest_hex: hex::encode(blake3::hash(turtle.as_bytes()).as_bytes()),
        sparql_projection_digest_hex: hex::encode(
            blake3::hash(RENDER_MODEL_PROJECTION_QUERY.as_bytes()).as_bytes(),
        ),
        tera_template_digest_hex: hex::encode(
            blake3::hash(ARAZZO_PROJECTION_TEMPLATE.as_bytes()).as_bytes(),
        ),
        arazzo_digest_hex: hex::encode(blake3::hash(arazzo_json.as_bytes()).as_bytes()),
        compiler_version: compiler_version.to_string(),
        air_digest_hex: hex::encode(air_digest.0),
        arazzo_document: arazzo_json,
    };
    Ok((outcome, air_wasm))
}

/// Decodes a 64-hex-character AIR digest string back into raw bytes. Only
/// ever called on a hex string [`render_and_compile`] just produced via
/// `hex::encode`, so a decode failure here indicates a crate defect, not
/// caller input.
fn decode_air_digest(hex_str: &str) -> Result<[u8; 32], CoreError> {
    let bytes = hex::decode(hex_str).map_err(|e| CoreError::ExternalCutCompilationFailed {
        stage: "air_compile".to_string(),
        detail: format!("AIR digest hex decode failed (crate defect): {e}"),
    })?;
    bytes
        .try_into()
        .map_err(|_| CoreError::ExternalCutCompilationFailed {
            stage: "air_compile".to_string(),
            detail: "AIR digest did not decode to exactly 32 bytes (crate defect)".to_string(),
        })
}

impl ArazzoProjectionReceipt {
    /// Rail A/B production pipeline (PROJ-796): runs the full real chain
    /// `A_z = T(Q(W))` (PRD.md sec.7.4) plus Rail B's Arazzo -> AIR
    /// lowering, normalization, and WASM compilation over one POWL region,
    /// admitting it first (via [`powl_to_turtle`]'s own
    /// `ExternalCutRefusal`-wrapping admission). Not a test-only helper:
    /// this runs the same real computation
    /// [`ChatmanRailAbCompiler::compile`] runs for
    /// `ChatmanEngine::admit_transition_with_external_cut` (both call
    /// [`render_and_compile`], the shared core), so a direct caller of this
    /// crate and the sealed engine's own admission path can never diverge.
    ///
    /// # Errors
    /// [`CoreError::ExternalCutCompilationFailed`] naming the failing
    /// sub-stage (`admission`, `sparql_projection`, `arazzo_parse`,
    /// `uri_resolution`, `air_lowering`, `air_normalization`,
    /// `air_compile`); [`CoreError::TemplateRenderFailed`] /
    /// [`CoreError::UnresolvedProjectionElement`] from
    /// [`render_arazzo_document`].
    ///
    /// # Complexity
    /// [`powl_to_turtle`]'s admission/emission bound (O(n + |order|) plus
    /// its own O(n) admission pass) plus [`render_and_compile`]'s bound.
    pub fn project_and_compile(
        region: &Powl,
        base_iri: &str,
        derived_from: Option<&str>,
        workflow_id: &str,
        title: &str,
        compiler_version: &str,
    ) -> Result<ArazzoCompilationArtifact, CoreError> {
        let turtle = powl_to_turtle(region, base_iri, derived_from).map_err(|e| {
            CoreError::ExternalCutCompilationFailed {
                stage: "admission".to_string(),
                detail: e.to_string(),
            }
        })?;
        let root_element_id = format!("{}/n0", base_iri.trim_end_matches('/'));
        let (outcome, air_wasm) = render_and_compile(
            &turtle,
            &root_element_id,
            workflow_id,
            title,
            compiler_version,
        )?;

        let receipt = ArazzoProjectionReceipt::from_materials(
            &turtle,
            &root_element_id,
            RENDER_MODEL_PROJECTION_QUERY,
            ARAZZO_PROJECTION_TEMPLATE,
            &outcome.arazzo_document,
            compiler_version,
            &outcome.air_digest_hex,
        );
        let air_digest = decode_air_digest(&outcome.air_digest_hex)?;
        Ok(ArazzoCompilationArtifact {
            receipt,
            arazzo_document: outcome.arazzo_document,
            air_digest,
            air_wasm,
        })
    }
}

/// The one real implementation of `praxis_graphlaw::chatman::
/// powl_projection::ExternalCutCompiler` this crate provides to
/// `ChatmanEngine::admit_transition_with_external_cut` -- injected because
/// `praxis-graphlaw` cannot depend on this crate (`praxis-core` already
/// depends on `praxis-graphlaw`; the reverse edge would be a cyclic crate
/// dependency), so the sealed engine depends on the trait seam instead and
/// a caller that already links both crates wires the concrete, real Rail
/// A/B pipeline in from here.
#[derive(Debug, Clone, Copy)]
pub struct ChatmanRailAbCompiler {
    /// Compiler version string folded into the receipt (mirrors
    /// `ArazzoProjectionReceipt::compiler_version`).
    pub compiler_version: &'static str,
}

impl Default for ChatmanRailAbCompiler {
    fn default() -> Self {
        Self {
            compiler_version: "26.7.11",
        }
    }
}

impl ExternalCutCompiler for ChatmanRailAbCompiler {
    fn compile(
        &self,
        request: &ExternalCutCompilationRequest<'_>,
    ) -> Result<ExternalCutCompilationOutcome, praxis_graphlaw::chatman::abi::Refusal> {
        let (outcome, _air_wasm) = render_and_compile(
            request.region_turtle,
            request.root_element_id,
            request.workflow_id,
            request.title,
            self.compiler_version,
        )
        .map_err(|e| {
            praxis_graphlaw::chatman::abi::Refusal::ValidationFailed(format!(
                "Rail A/B compilation failed: {e}"
            ))
        })?;
        Ok(outcome)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn arazzo_projection_receipt_digest_is_deterministic() {
        let receipt = ArazzoProjectionReceipt {
            source_powl_digest_hex: "00000000000000000000000000000001".to_string(),
            external_cut_identity: "cut-123".to_string(),
            sparql_projection_digest_hex: "00000000000000000000000000000002".to_string(),
            tera_template_digest_hex: "00000000000000000000000000000003".to_string(),
            arazzo_digest_hex: "00000000000000000000000000000004".to_string(),
            compiler_version: "v26.7.11".to_string(),
            air_digest_hex: "00000000000000000000000000000005".to_string(),
        };

        let digest1 = receipt.compute_digest().unwrap();
        let digest2 = receipt.compute_digest().unwrap();
        assert_eq!(digest1, digest2);
    }

    // ── PROJ-752: real T-stage render + full round-trip ─────────────────
    //
    // `Powl`, `powl_to_turtle`, `run_render_model_projection`,
    // `RENDER_MODEL_PROJECTION_QUERY`, and `DocumentIndex` are all already
    // in scope via `use super::*;` above (PROJ-796 promoted them to
    // module-level production imports).

    const TEST_PROJECTION: &str = "SELECT * WHERE { ?s ?p ?o }";
    const TEST_RENDERER: &str = "arazzo_projection.tera";

    /// Mirrors `powl_projection::tests::model_with_external_cut` (PROJ-751):
    /// a two-step `PartialOrder` whose second child is an `ExternalCut`, so
    /// this test exercises both step kinds [`render_arazzo_document`] must
    /// handle, not just the trivial all-leaves case.
    fn model_with_external_cut() -> Powl {
        Powl::PartialOrder {
            children: vec![
                Powl::Leaf(Some("intake".to_string())),
                Powl::ExternalCut {
                    region: Box::new(Powl::Leaf(Some("remote_settle".to_string()))),
                    projection: TEST_PROJECTION.to_string(),
                    renderer: TEST_RENDERER.to_string(),
                },
            ],
            order: std::collections::BTreeSet::from([(0usize, 1usize)]),
        }
    }

    /// The full Rail A round trip this ticket requires: an admitted POWL
    /// region (W) -> real SPARQL execution (Q, PROJ-751) -> real Tera
    /// render (T, this ticket) -> a manufactured Arazzo JSON document that
    /// actually parses via wasm4pm-arazzo's own `DocumentIndex`, not a
    /// hand-typed fixture standing in for one.
    #[test]
    fn manufactured_arazzo_round_trips_through_wasm4pm_arazzo_parser(
    ) -> Result<(), Box<dyn std::error::Error>> {
        let model = model_with_external_cut();
        let base_iri = "urn:test:proj752";
        let turtle = powl_to_turtle(&model, base_iri, None)?;

        let rows = run_render_model_projection(&turtle)?;
        assert!(
            !rows.is_empty(),
            "PROJ-751's real SPARQL execution must yield rows over this fixture"
        );

        let root_element_id = format!("{base_iri}/n0");
        let arazzo_json = render_arazzo_document(
            &rows,
            &root_element_id,
            "manufactured-rail-a-workflow",
            "Rail A manufactured workflow (PROJ-752)",
        )?;

        // The rendered text must be valid JSON on its own terms (not merely
        // "parses as an ArazzoDescription", which would hide a JSON-shape
        // bug behind serde's leniency).
        let _: serde_json::Value = serde_json::from_str(&arazzo_json)?;

        // Real parse via wasm4pm-arazzo's own admission path -- proves this
        // is a real Arazzo 1.1.x document, not merely JSON shaped like one.
        let mut index = DocumentIndex::new();
        index.add_document(&arazzo_json, "urn:test:proj752/manufactured")?;
        assert_eq!(index.documents.len(), 1);
        let parsed = index
            .documents
            .get("urn:test:proj752/manufactured")
            .expect("just inserted under this exact base_uri");
        assert_eq!(parsed.arazzo, "1.1.0");
        assert_eq!(parsed.workflows.len(), 1);
        // Both the plain leaf ("intake") and the external-cut boundary
        // step round-tripped -- not just whichever one happens to serialize
        // without a template bug.
        assert_eq!(parsed.workflows[0].steps.len(), 2);
        assert!(parsed.workflows[0]
            .steps
            .iter()
            .any(|s| s.description.as_deref() == Some("intake")));
        assert!(parsed.workflows[0]
            .steps
            .iter()
            .any(|s| s.extensions.contains_key("x-powl-external-cut")));

        // PROJ-753: resolve URIs, lower the parsed document into AIR, normalize
        // (resolve any cross-step Variable references), and compile to WASM --
        // completing the Rail A/B seam that this test previously stopped short
        // of (`air_digest_hex` used to be a hand-typed "unavailable:PROJ-753"
        // sentinel because no lowering function existed anywhere in the repo).
        wasm4pm_arazzo::resolve::normalize_uris(&mut index)?;
        let parsed = index
            .documents
            .get("urn:test:proj752/manufactured")
            .expect("still inserted under this exact base_uri after URI normalization");

        let bump = bumpalo::Bump::new();
        let mut air_program = wasm4pm_arazzo::lower::lower_description(parsed, &bump)?;
        wasm4pm_arazzo::normalizer::ArazzoNormalizer::normalize(&mut air_program, &bump)?;
        let air_wasm = wasm4pm_arazzo::compile::AirCompiler::compile_to_wasm(&air_program)?;
        let air_digest = wasm4pm_arazzo::compile::AirCompiler::digest_program(&air_program)?;

        // The compiled AIR module must carry the real POWL element IDs this
        // projection produced (the manufactured document's own operationId
        // values -- "intake"'s leaf and the external cut's boundary node), not
        // a hand-typed fixture string.
        let intake_operation_id = format!("{root_element_id}/c0");
        let external_cut_operation_id = format!("{root_element_id}/c1");
        assert!(
            contains_subslice(&air_wasm, intake_operation_id.as_bytes()),
            "compiled AIR module must contain the real intake step's operationId"
        );
        assert!(
            contains_subslice(&air_wasm, external_cut_operation_id.as_bytes()),
            "compiled AIR module must contain the real external-cut step's operationId"
        );

        // Receipt: every digest field is now a real BLAKE3 digest of real
        // materials this test just produced -- air_digest_hex included, no
        // placeholder strings anywhere.
        let receipt = ArazzoProjectionReceipt::from_materials(
            &turtle,
            &root_element_id,
            RENDER_MODEL_PROJECTION_QUERY,
            ARAZZO_PROJECTION_TEMPLATE,
            &arazzo_json,
            "26.7.11",
            &hex::encode(air_digest.0),
        );
        assert_eq!(
            receipt.source_powl_digest_hex,
            hex::encode(blake3::hash(turtle.as_bytes()).as_bytes())
        );
        assert_eq!(
            receipt.sparql_projection_digest_hex,
            hex::encode(blake3::hash(RENDER_MODEL_PROJECTION_QUERY.as_bytes()).as_bytes())
        );
        assert_eq!(
            receipt.tera_template_digest_hex,
            hex::encode(blake3::hash(ARAZZO_PROJECTION_TEMPLATE.as_bytes()).as_bytes())
        );
        assert_eq!(
            receipt.arazzo_digest_hex,
            hex::encode(blake3::hash(arazzo_json.as_bytes()).as_bytes())
        );
        assert_eq!(receipt.air_digest_hex, hex::encode(air_digest.0));
        let digest1 = receipt.compute_digest()?;
        let digest2 = receipt.compute_digest()?;
        assert_eq!(digest1, digest2, "receipt digest must be deterministic");

        Ok(())
    }

    fn contains_subslice(haystack: &[u8], needle: &[u8]) -> bool {
        !needle.is_empty() && haystack.windows(needle.len()).any(|w| w == needle)
    }

    #[test]
    fn render_arazzo_document_refuses_unresolved_root() {
        let result = render_arazzo_document(&[], "urn:test:proj752-negative/n0", "wf", "title");
        assert!(matches!(
            result,
            Err(CoreError::UnresolvedProjectionElement(_))
        ));
    }

    // ── admit_manufactured_arazzo (PROJ-783: ARAZZO_UNMANUFACTURED /
    // ── ARAZZO_SOURCE_RECEIPT_MISSING / ARAZZO_PROJECTION_DIGEST_MISMATCH)
    //
    // All four scenarios run the real Rail A/B pipeline
    // (`ArazzoProjectionReceipt::project_and_compile`) to get a genuine
    // manufactured document + receipt, then exercise the admission gate
    // over that real output -- never a hand-typed fixture standing in for
    // one.

    fn real_manufactured_artifact() -> Result<ArazzoCompilationArtifact, Box<dyn std::error::Error>>
    {
        let model = model_with_external_cut();
        Ok(ArazzoProjectionReceipt::project_and_compile(
            &model,
            "urn:test:proj783",
            Some("urn:test:proj783"),
            "manufactured-admission-workflow",
            "PROJ-783 manufacture admission test",
            "26.7.11",
        )?)
    }

    #[test]
    fn admit_manufactured_arazzo_accepts_a_real_manufactured_document(
    ) -> Result<(), Box<dyn std::error::Error>> {
        let artifact = real_manufactured_artifact()?;
        admit_manufactured_arazzo(&artifact.arazzo_document, Some(&artifact.receipt))?;
        Ok(())
    }

    #[test]
    fn admit_manufactured_arazzo_refuses_a_document_with_no_receipt(
    ) -> Result<(), Box<dyn std::error::Error>> {
        let artifact = real_manufactured_artifact()?;
        let result = admit_manufactured_arazzo(&artifact.arazzo_document, None);
        assert!(
            matches!(&result, Err(CoreError::ArazzoUnmanufactured(_))),
            "a document presented with no receipt at all must refuse \
             ArazzoUnmanufactured, got: {result:?}"
        );
        Ok(())
    }

    #[test]
    fn admit_manufactured_arazzo_refuses_a_receipt_with_no_source_binding(
    ) -> Result<(), Box<dyn std::error::Error>> {
        let artifact = real_manufactured_artifact()?;
        let mut unbound_receipt = artifact.receipt.clone();
        unbound_receipt.source_powl_digest_hex = String::new();
        let result = admit_manufactured_arazzo(&artifact.arazzo_document, Some(&unbound_receipt));
        assert!(
            matches!(&result, Err(CoreError::ArazzoSourceReceiptMissing(_))),
            "a receipt with an empty source_powl_digest_hex must refuse \
             ArazzoSourceReceiptMissing, got: {result:?}"
        );
        Ok(())
    }

    #[test]
    fn admit_manufactured_arazzo_refuses_a_tampered_document(
    ) -> Result<(), Box<dyn std::error::Error>> {
        let artifact = real_manufactured_artifact()?;
        // The receipt was cut over `artifact.arazzo_document`; presenting a
        // byte-different document under the same receipt must be refused.
        let tampered = format!(
            "{}\n// tampered after manufacture\n",
            artifact.arazzo_document
        );
        let result = admit_manufactured_arazzo(&tampered, Some(&artifact.receipt));
        assert!(
            matches!(&result, Err(CoreError::ArazzoProjectionDigestMismatch(_))),
            "a document that no longer recomputes to the receipt's arazzo_digest_hex must \
             refuse ArazzoProjectionDigestMismatch, got: {result:?}"
        );
        Ok(())
    }

    // ── admit_manufactured_arazzo_for_dialect (PROJ-777/778: dialect
    // ── authority gate wired against graphlaw_authority::REGISTRY) ────────

    #[test]
    fn admit_manufactured_arazzo_for_dialect_accepts_the_arazzo_dialect(
    ) -> Result<(), Box<dyn std::error::Error>> {
        let artifact = real_manufactured_artifact()?;
        admit_manufactured_arazzo_for_dialect(
            "Arazzo",
            &artifact.arazzo_document,
            Some(&artifact.receipt),
        )?;
        Ok(())
    }

    #[test]
    fn admit_manufactured_arazzo_for_dialect_refuses_an_unregistered_dialect_name(
    ) -> Result<(), Box<dyn std::error::Error>> {
        let artifact = real_manufactured_artifact()?;
        let result = admit_manufactured_arazzo_for_dialect(
            "not-a-real-dialect",
            &artifact.arazzo_document,
            Some(&artifact.receipt),
        );
        assert!(
            matches!(
                &result,
                Err(CoreError::ArazzoDialectAuthorityMismatch { declared, expected, .. })
                    if declared == "not-a-real-dialect" && *expected == "Arazzo"
            ),
            "an unregistered dialect name must refuse ArazzoDialectAuthorityMismatch, \
             got: {result:?}"
        );
        Ok(())
    }

    #[test]
    fn admit_manufactured_arazzo_for_dialect_refuses_a_different_registered_dialect(
    ) -> Result<(), Box<dyn std::error::Error>> {
        // PROJ-778's core claim, exercised end-to-end: a document that is
        // otherwise a byte-perfect, receipt-matched manufactured Arazzo
        // artifact still must not be admitted if the caller declares it
        // under "SPARQL CONSTRUCT" -- a real, registered dialect whose own
        // authority ("manufacture graph consequence") is not Arazzo's. No
        // dialect acquires Arazzo's authority merely by being *presented*
        // alongside content that would otherwise pass Arazzo's own checks.
        let artifact = real_manufactured_artifact()?;
        let result = admit_manufactured_arazzo_for_dialect(
            "SPARQL CONSTRUCT",
            &artifact.arazzo_document,
            Some(&artifact.receipt),
        );
        assert!(
            matches!(
                &result,
                Err(CoreError::ArazzoDialectAuthorityMismatch { declared, expected, .. })
                    if declared == "SPARQL CONSTRUCT" && *expected == "Arazzo"
            ),
            "declaring a real but different dialect must refuse \
             ArazzoDialectAuthorityMismatch, not fall through to Arazzo's own checks, \
             got: {result:?}"
        );
        Ok(())
    }
}
