//! Reflects an OpenAPI 3.x document into a `cnv:Cli` RDF graph.
//!
//! 80/20 slice (see crate-level docs / the plan this crate implements):
//! `GET` operations only, scalar (`string`/`integer`/`number`/`boolean`)
//! path and query parameters only, every reflected command carries
//! `cnv:CustomBehavior` (a real HTTP call is never one of the six closed
//! primitives). Anything outside that slice is skipped with a collected
//! warning, never silently dropped.

use oxigraph::model::{GraphNameRef, Literal, NamedNode, QuadRef};
use oxigraph::store::Store;
use serde_json::Value;

use crate::error::ReflectError;
use crate::naming::{kebab_case, snake_case};

const NS: &str = "https://clap-noun-verb.dev/ontology#";

fn iri(local: &str) -> NamedNode {
    NamedNode::new_unchecked(format!("{NS}{local}"))
}

fn xsd(local: &str) -> NamedNode {
    NamedNode::new_unchecked(format!("http://www.w3.org/2001/XMLSchema#{local}"))
}

/// One operation the reflector declined to translate, and why.
#[derive(Debug, Clone)]
pub struct ReflectWarning {
    pub path: String,
    pub method: String,
    pub reason: String,
}

/// Successful reflection result: the populated graph plus whatever was
/// skipped along the way.
pub struct ReflectOutcome {
    pub store: Store,
    pub warnings: Vec<ReflectWarning>,
}

struct Graph {
    store: Store,
}

impl Graph {
    fn new() -> Result<Self, ReflectError> {
        Ok(Self {
            store: Store::new().map_err(|e| ReflectError::Store(e.to_string()))?,
        })
    }

    fn insert(&self, s: &NamedNode, p: &NamedNode, o: &NamedNode) -> Result<(), ReflectError> {
        self.store
            .insert(QuadRef::new(s, p, o, GraphNameRef::DefaultGraph))
            .map(|_| ())
            .map_err(|e| ReflectError::Store(e.to_string()))
    }

    fn insert_string(&self, s: &NamedNode, p: &NamedNode, value: &str) -> Result<(), ReflectError> {
        let literal = Literal::new_simple_literal(value);
        self.store
            .insert(QuadRef::new(s, p, &literal, GraphNameRef::DefaultGraph))
            .map(|_| ())
            .map_err(|e| ReflectError::Store(e.to_string()))
    }

    fn insert_typed(
        &self, s: &NamedNode, p: &NamedNode, value: &str, datatype: &str,
    ) -> Result<(), ReflectError> {
        let literal = Literal::new_typed_literal(value, xsd(datatype));
        self.store
            .insert(QuadRef::new(s, p, &literal, GraphNameRef::DefaultGraph))
            .map(|_| ())
            .map_err(|e| ReflectError::Store(e.to_string()))
    }
}

/// A single reflected path/query parameter, already validated against the
/// 80/20 slice's scalar `valueKind` set.
struct ScalarParam {
    field: String,
    value_kind: &'static str,
    required: bool,
    is_path: bool,
    about: String,
}

fn map_schema_type(schema_type: &str) -> Option<&'static str> {
    match schema_type {
        "string" => Some("string"),
        "integer" => Some("i64"),
        "number" => Some("f64"),
        "boolean" => Some("bool"),
        _ => None,
    }
}

fn synthetic_test_value(value_kind: &str) -> &'static str {
    match value_kind {
        "i64" => "1",
        "f64" => "1.0",
        "bool" => "true",
        _ => "example",
    }
}

/// Collect and validate one operation's `parameters[]`. Returns `None` (with
/// a warning appended to `warnings`) if any parameter falls outside the
/// scalar-type 80/20 slice, or if a `parameters[]` entry is malformed.
fn collect_scalar_params(
    path: &str, method: &str, operation: &Value, warnings: &mut Vec<ReflectWarning>,
) -> Option<Vec<ScalarParam>> {
    let empty = Vec::new();
    let params = operation
        .get("parameters")
        .and_then(Value::as_array)
        .unwrap_or(&empty);

    let mut result = Vec::with_capacity(params.len());
    for param in params {
        let Some(name) = param.get("name").and_then(Value::as_str) else {
            warnings.push(ReflectWarning {
                path: path.to_owned(),
                method: method.to_owned(),
                reason: "parameter missing a \"name\"".to_owned(),
            });
            return None;
        };
        let location = param.get("in").and_then(Value::as_str).unwrap_or("");
        let is_path = match location {
            "path" => true,
            "query" => false,
            other => {
                warnings.push(ReflectWarning {
                    path: path.to_owned(),
                    method: method.to_owned(),
                    reason: format!("parameter {name:?} has unsupported location \"{other}\" (only path/query are reflected)"),
                });
                return None;
            }
        };
        let schema_type = param
            .get("schema")
            .and_then(|s| s.get("type"))
            .and_then(Value::as_str)
            .unwrap_or("");
        let Some(value_kind) = map_schema_type(schema_type) else {
            warnings.push(ReflectWarning {
                path: path.to_owned(),
                method: method.to_owned(),
                reason: format!(
                    "parameter {name:?} has non-scalar or missing schema type {schema_type:?} \
                     (only string/integer/number/boolean are reflected)"
                ),
            });
            return None;
        };
        if is_path && value_kind == "bool" {
            // gates/110_macro_surface.rq refuses a positional bool outright
            // (booleans are always flags, never positional, in this closed
            // argument model).
            warnings.push(ReflectWarning {
                path: path.to_owned(),
                method: method.to_owned(),
                reason: format!(
                    "parameter {name:?} is a boolean path parameter, which the closed \
                     argument model refuses (booleans are always flags, never positional)"
                ),
            });
            return None;
        }
        let required = is_path
            || param
                .get("required")
                .and_then(Value::as_bool)
                .unwrap_or(false);
        if !is_path && value_kind == "bool" && required {
            // gates/100_argument_relationships.rq refuses a required boolean
            // flag outright (booleans are presence flags in this closed
            // argument model) -- skip rather than hand the compiler a
            // provably-refused ontology.
            warnings.push(ReflectWarning {
                path: path.to_owned(),
                method: method.to_owned(),
                reason: format!(
                    "parameter {name:?} is a required boolean query parameter, which the \
                     closed argument model refuses (booleans are optional presence flags)"
                ),
            });
            return None;
        }
        let about = param
            .get("description")
            .and_then(Value::as_str)
            .map(str::to_owned)
            .unwrap_or_else(|| format!("Reflected from OpenAPI parameter {name}."));

        result.push(ScalarParam {
            field: snake_case(name),
            value_kind,
            required,
            is_path,
            about,
        });
    }
    Some(result)
}

/// Reflect an OpenAPI 3.x JSON document into a `cnv:Cli` RDF graph.
///
/// # Errors
/// Returns [`ReflectError::NotOpenApi`] if the document doesn't have a
/// `paths` object at all, or [`ReflectError::NothingReflected`] if every
/// operation fell outside the 80/20 slice (see module docs).
pub fn reflect(spec: &Value) -> Result<ReflectOutcome, ReflectError> {
    let paths = spec
        .get("paths")
        .and_then(Value::as_object)
        .ok_or_else(|| ReflectError::NotOpenApi("no top-level \"paths\" object".to_owned()))?;

    let title = spec
        .get("info")
        .and_then(|i| i.get("title"))
        .and_then(Value::as_str)
        .unwrap_or("Reflected CLI");
    let raw_version = spec
        .get("info")
        .and_then(|i| i.get("version"))
        .and_then(Value::as_str)
        .unwrap_or("");
    let semver_re_ok = raw_version.split('.').count() == 3
        && raw_version
            .split('.')
            .all(|part| !part.is_empty() && part.chars().all(|c| c.is_ascii_digit()));
    let version = if semver_re_ok { raw_version } else { "0.1.0" };
    let crate_name = kebab_case(title);

    let graph = Graph::new()?;
    let mut warnings = Vec::new();

    let cli = iri(&format!("Cli_{}", snake_case(&crate_name)));
    let rdf_type = NamedNode::new_unchecked("http://www.w3.org/1999/02/22-rdf-syntax-ns#type");
    graph.insert(&cli, &rdf_type, &iri("Cli"))?;
    graph.insert_string(&cli, &iri("binaryName"), &crate_name)?;
    graph.insert_string(&cli, &iri("crateName"), &crate_name)?;
    graph.insert_string(&cli, &iri("version"), version)?;
    graph.insert_string(&cli, &iri("edition"), "2024")?;
    graph.insert_string(&cli, &iri("rustVersion"), "1.85")?;
    graph.insert_string(
        &cli,
        &iri("about"),
        &format!("Reflected from OpenAPI document {title:?}."),
    )?;

    // noun name -> (noun IRI, command IRIs)
    let mut nouns: std::collections::BTreeMap<String, (NamedNode, Vec<NamedNode>)> =
        std::collections::BTreeMap::new();

    for (path, path_item) in paths {
        let Some(path_item) = path_item.as_object() else {
            continue;
        };
        const HTTP_METHODS: [&str; 7] =
            ["get", "put", "post", "delete", "patch", "head", "options"];
        for (method, operation) in path_item {
            if !HTTP_METHODS.contains(&method.as_str()) {
                continue; // not an operation at all (e.g. a path-level "parameters"/"summary")
            }
            if method != "get" {
                warnings.push(ReflectWarning {
                    path: path.clone(),
                    method: method.clone(),
                    reason: "only GET operations are reflected in this 80/20 slice".to_owned(),
                });
                continue;
            }
            let Some(operation_id) = operation.get("operationId").and_then(Value::as_str) else {
                warnings.push(ReflectWarning {
                    path: path.clone(),
                    method: method.clone(),
                    reason: "operation has no \"operationId\"".to_owned(),
                });
                continue;
            };

            let Some(params) = collect_scalar_params(path, method, operation, &mut warnings) else {
                continue;
            };

            let noun_name = operation
                .get("tags")
                .and_then(Value::as_array)
                .and_then(|tags| tags.first())
                .and_then(Value::as_str)
                .map(snake_case)
                .filter(|n| n != "root")
                .unwrap_or_else(|| {
                    path.trim_start_matches('/')
                        .split('/')
                        .find(|segment| !segment.starts_with('{'))
                        .map(snake_case)
                        .filter(|n| !n.is_empty() && n != "root")
                        .unwrap_or_else(|| "default".to_owned())
                });
            let verb_name = snake_case(operation_id);

            let noun_iri = iri(&format!("Noun_{noun_name}"));
            let command_iri = iri(&format!("Command_{noun_name}_{verb_name}"));
            let behavior_iri = iri(&format!("Behavior_{noun_name}_{verb_name}"));

            let about = operation
                .get("summary")
                .or_else(|| operation.get("description"))
                .and_then(Value::as_str)
                .map(str::to_owned)
                .unwrap_or_else(|| format!("Reflected from OpenAPI operation GET {path}."));

            graph.insert(&command_iri, &rdf_type, &iri("Command"))?;
            // cnv:name is gated to ^[a-z][a-z0-9-]*$ (hyphens, no underscores) --
            // distinct from cnv:fieldName's ^[a-z][a-z0-9_]*$ (underscores).
            graph.insert_string(&command_iri, &iri("name"), &verb_name.replace('_', "-"))?;
            graph.insert_string(&command_iri, &iri("about"), &about)?;
            graph.insert(&command_iri, &iri("belongsToNoun"), &noun_iri)?;
            graph.insert(&command_iri, &iri("hasBehavior"), &behavior_iri)?;

            graph.insert(&behavior_iri, &rdf_type, &iri("CustomBehavior"))?;

            let mut next_position = 1u32;
            for param in &params {
                let argument_iri =
                    iri(&format!("Argument_{noun_name}_{verb_name}_{}", param.field));
                graph.insert(&argument_iri, &rdf_type, &iri("Argument"))?;
                graph.insert_string(&argument_iri, &iri("name"), &param.field.replace('_', "-"))?;
                graph.insert_string(&argument_iri, &iri("fieldName"), &param.field)?;
                graph.insert_string(&argument_iri, &iri("valueKind"), param.value_kind)?;
                graph.insert_typed(
                    &argument_iri,
                    &iri("required"),
                    if param.required { "true" } else { "false" },
                    "boolean",
                )?;
                graph.insert_string(&argument_iri, &iri("about"), &param.about)?;
                graph.insert_string(
                    &argument_iri,
                    &iri("testValue"),
                    synthetic_test_value(param.value_kind),
                )?;
                if param.is_path {
                    graph.insert_typed(
                        &argument_iri,
                        &iri("position"),
                        &next_position.to_string(),
                        "integer",
                    )?;
                    next_position += 1;
                } else {
                    graph.insert_typed(&argument_iri, &iri("position"), "0", "integer")?;
                    graph.insert_string(
                        &argument_iri,
                        &iri("longFlag"),
                        &param.field.replace('_', "-"),
                    )?;
                }
                graph.insert(&command_iri, &iri("hasArgument"), &argument_iri)?;
            }

            let entry = nouns
                .entry(noun_name.clone())
                .or_insert_with(|| (noun_iri.clone(), Vec::new()));
            entry.1.push(command_iri);
        }
    }

    if nouns.is_empty() {
        return Err(ReflectError::NothingReflected);
    }

    for (noun_name, (noun_iri, command_iris)) in &nouns {
        graph.insert(noun_iri, &rdf_type, &iri("Noun"))?;
        graph.insert_string(noun_iri, &iri("name"), &noun_name.replace('_', "-"))?;
        graph.insert_string(
            noun_iri,
            &iri("about"),
            &format!("Reflected OpenAPI operations grouped under {noun_name:?}."),
        )?;
        for command_iri in command_iris {
            graph.insert(noun_iri, &iri("hasCommand"), command_iri)?;
        }
        graph.insert(&cli, &iri("hasNoun"), noun_iri)?;
    }

    Ok(ReflectOutcome {
        store: graph.store,
        warnings,
    })
}
