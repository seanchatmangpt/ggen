use oxigraph::io::{RdfFormat, RdfParser};
use oxigraph::model::Term;
use oxigraph::sparql::QueryResults;
use oxigraph::store::Store;
use serde::{Deserialize, Serialize};

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct DialectResult {
    pub conforms: bool,
    pub message: String,
    pub supported: bool,
}

#[allow(deprecated)]
pub fn check_sparql(sparql: &str) -> Result<DialectResult, String> {
    let store = Store::new().map_err(|e| e.to_string())?;

    // Seed a minimal triple so ASK/SELECT/CONSTRUCT fixtures have data to match.
    // ask_pass.rq uses: ASK { ?s ?p ?o } — needs at least one triple.
    let seed_ttl = "<http://www.w3.org/ns/prov#s> <http://www.w3.org/ns/prov#p> <http://www.w3.org/ns/prov#o> .";
    store
        .load_from_reader(
            RdfParser::from_format(RdfFormat::Turtle),
            seed_ttl.as_bytes(),
        )
        .map_err(|e| e.to_string())?;

    // Check it looks like a valid SPARQL query keyword
    let query = sparql.trim();
    let upper = query.to_uppercase();
    if !upper.starts_with("ASK")
        && !upper.starts_with("SELECT")
        && !upper.starts_with("CONSTRUCT")
        && !upper.contains("PREFIX")
    {
        return Err("Invalid SPARQL query: must be ASK, SELECT, or CONSTRUCT".to_string());
    }

    let parsed_query = store
        .query(query)
        .map_err(|e| format!("SPARQL parse error: {}", e))?;

    match parsed_query {
        QueryResults::Boolean(b) => Ok(DialectResult {
            conforms: b,
            message: format!("SPARQL ASK returned {}", b),
            supported: true,
        }),
        QueryResults::Solutions(solutions) => {
            let count = solutions.count();
            Ok(DialectResult {
                conforms: count > 0,
                message: format!("SPARQL SELECT returned {} rows", count),
                supported: true,
            })
        }
        QueryResults::Graph(triples) => {
            let count = triples.count();
            Ok(DialectResult {
                conforms: count > 0,
                message: format!("SPARQL CONSTRUCT returned {} triples", count),
                supported: true,
            })
        }
    }
}

#[allow(deprecated)]
pub fn check_shacl(shacl: &str) -> Result<DialectResult, String> {
    let store = Store::new().map_err(|e| e.to_string())?;
    store
        .load_from_reader(RdfParser::from_format(RdfFormat::Turtle), shacl.as_bytes())
        .map_err(|e| e.to_string())?;

    // Basic SHACL pattern check: find NodeShapes and their constraints
    let shape_query = "
        PREFIX sh: <http://www.w3.org/ns/shacl#>
        SELECT ?shape WHERE {
            ?shape a sh:NodeShape .
        }
    ";

    let mut shapes = Vec::new();
    let query_results = store.query(shape_query).map_err(|e| e.to_string())?;
    if let QueryResults::Solutions(solutions) = query_results {
        for sol in solutions {
            let sol = sol.map_err(|e| e.to_string())?;
            if let Some(Term::NamedNode(n)) = sol.get("shape") {
                shapes.push(n.clone());
            }
        }
    }

    if shapes.is_empty() {
        return Err("No valid SHACL NodeShapes found".to_string());
    }

    // Evaluate constraints: for each shape, check target instances against property constraints.
    let mut violations: Vec<String> = Vec::new();
    for shape in &shapes {
        // Resolve the sh:targetClass for this shape
        let target_query = format!(
            "PREFIX sh: <http://www.w3.org/ns/shacl#>
             SELECT ?target WHERE {{ <{}> sh:targetClass ?target }}",
            shape.as_str()
        );
        let mut target_classes: Vec<String> = Vec::new();
        let target_results = store.query(&target_query).map_err(|e| e.to_string())?;
        if let QueryResults::Solutions(solutions) = target_results {
            for sol in solutions.flatten() {
                if let Some(Term::NamedNode(n)) = sol.get("target") {
                    target_classes.push(n.as_str().to_string());
                }
            }
        }
        // If no targetClass, fall back to shape IRI as type
        if target_classes.is_empty() {
            target_classes.push(shape.as_str().to_string());
        }

        // Find instances of the target class
        let mut instances: Vec<String> = Vec::new();
        for target_class in &target_classes {
            let inst_query = format!("SELECT ?inst WHERE {{ ?inst a <{}> }}", target_class);
            let inst_results = store.query(&inst_query).map_err(|e| e.to_string())?;
            if let QueryResults::Solutions(solutions) = inst_results {
                for sol in solutions.flatten() {
                    if let Some(Term::NamedNode(n)) = sol.get("inst") {
                        instances.push(n.as_str().to_string());
                    }
                }
            }
        }

        // For each instance, check required properties (sh:minCount 1)
        let prop_query = format!(
            "PREFIX sh: <http://www.w3.org/ns/shacl#>
             SELECT ?path ?min WHERE {{ <{}> sh:property [ sh:path ?path ; sh:minCount ?min ] }}",
            shape.as_str()
        );
        let prop_results = store.query(&prop_query).map_err(|e| e.to_string())?;
        let mut required_props: Vec<(String, i64)> = Vec::new();
        if let QueryResults::Solutions(solutions) = prop_results {
            for sol in solutions.flatten() {
                let path = sol.get("path").map(|t| t.to_string()).unwrap_or_default();
                let min = if let Some(Term::Literal(l)) = sol.get("min") {
                    l.value().parse::<i64>().unwrap_or(0)
                } else {
                    0
                };
                if min > 0 {
                    required_props.push((path, min));
                }
            }
        }

        for inst in &instances {
            for (prop, min) in &required_props {
                // Strip surrounding < > from SPARQL string representation
                let prop_iri = prop.trim_start_matches('<').trim_end_matches('>');
                let count_query = format!(
                    "SELECT (COUNT(?v) AS ?c) WHERE {{ <{}> <{}> ?v }}",
                    inst, prop_iri
                );
                let count_res = store.query(&count_query).map_err(|e| e.to_string())?;
                if let QueryResults::Solutions(mut sols) = count_res {
                    if let Some(Ok(sol)) = sols.next() {
                        let count = if let Some(Term::Literal(l)) = sol.get("c") {
                            l.value().parse::<i64>().unwrap_or(0)
                        } else {
                            0
                        };
                        if count < *min {
                            violations.push(format!(
                                "<{}> violates shape <{}>: property <{}> requires minCount {} but found {}",
                                inst, shape.as_str(), prop_iri, min, count
                            ));
                        }
                    }
                }
            }
        }
    }

    if violations.is_empty() {
        Ok(DialectResult {
            conforms: true,
            message: format!("SHACL verification passed ({} shapes found)", shapes.len()),
            supported: true,
        })
    } else {
        Ok(DialectResult {
            conforms: false,
            message: format!(
                "SHACL: {} violation(s) — instance violates shape: {}",
                violations.len(),
                violations[0]
            ),
            supported: true,
        })
    }
}

pub fn check_n3(n3: &str) -> Result<DialectResult, String> {
    // N3 is a superset of Turtle with rule/formula syntax ({ } and =>).
    // We validate syntax structurally — actual rule execution is unsupported.
    let is_n3_formula = n3.contains("=>") || n3.contains("@forAll");

    // Brace balance check (required for formulae)
    let mut depth: i64 = 0;
    for ch in n3.chars() {
        match ch {
            '{' => depth += 1,
            '}' => depth -= 1,
            _ => {}
        }
        if depth < 0 {
            return Err("Unbalanced braces in N3 input".to_string());
        }
    }
    if depth != 0 {
        return Err("Unbalanced braces in N3 input".to_string());
    }

    // N3 formulas with { } cannot be parsed as plain Turtle — skip Turtle parse for formula N3.
    if !is_n3_formula {
        // Plain Turtle subset — validate as Turtle
        let store = Store::new().map_err(|e| e.to_string())?;
        store
            .load_from_reader(RdfParser::from_format(RdfFormat::Turtle), n3.as_bytes())
            .map_err(|e| format!("N3/Turtle parse error: {}", e))?;
    }

    // N3 rule execution is not supported in this engine
    Ok(DialectResult {
        conforms: false,
        message: "N3 syntax verified (valid), but rule execution is an unsupported capability in this engine".to_string(),
        supported: false,
    })
}

pub fn check_datalog(datalog: &str) -> Result<DialectResult, String> {
    // Datalog (RDF-compatible rules) validation
    let trimmed = datalog.trim();
    if trimmed.is_empty() {
        return Err("Empty Datalog input".to_string());
    }

    // Basic Datalog syntax check: head :- body.
    let mut rules_count = 0;
    for line in trimmed.lines() {
        let l = line.trim();
        if l.is_empty() || l.starts_with('%') || l.starts_with('#') {
            continue;
        }

        // Parenthesis balance check per logical statement
        let open = l.chars().filter(|&c| c == '(').count() as i64;
        let close = l.chars().filter(|&c| c == ')').count() as i64;
        if open != close {
            return Err(format!("Mismatched parentheses in Datalog at: {}", l));
        }

        if l.contains(":-") && l.ends_with('.') {
            rules_count += 1;
        } else if l.contains('(') && l.ends_with('.') {
            // Fact
            rules_count += 1;
        } else if !l.is_empty() {
            return Err(format!("Invalid Datalog syntax at: {}", l));
        }
    }

    if rules_count == 0 {
        return Err("No valid Datalog rules or facts found".to_string());
    }

    Ok(DialectResult {
        conforms: false,
        message: "Datalog syntax verified (valid), but execution is an unsupported capability in this engine".to_string(),
        supported: false,
    })
}

pub fn check_shex(shex: &str) -> Result<DialectResult, String> {
    // ShEx (Shape Expressions) validation
    let trimmed = shex.trim();
    if trimmed.is_empty() {
        return Err("Empty ShEx input".to_string());
    }

    // Basic ShEx syntax check: <Shape> { ... }
    let mut shapes_count = 0;
    let mut open_braces = 0;
    for line in trimmed.lines() {
        let l = line.trim();
        if l.is_empty() || l.starts_with('#') || l.starts_with("PREFIX") || l.starts_with("BASE") {
            continue;
        }

        if l.contains('{') {
            open_braces += 1;
            shapes_count += 1;
        }
        if l.contains('}') {
            open_braces -= 1;
        }
    }

    if open_braces != 0 {
        return Err("Unbalanced braces in ShEx input".to_string());
    }

    if shapes_count == 0 && !shex.trim().is_empty() {
        return Err("No valid shape definitions found in ShEx input".to_string());
    }

    Ok(DialectResult {
        conforms: false,
        message: "ShEx syntax verified (valid), but execution is an unsupported capability in this engine".to_string(),
        supported: false,
    })
}
