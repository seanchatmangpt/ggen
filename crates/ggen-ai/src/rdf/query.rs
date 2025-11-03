//! SPARQL query execution for RDF data
//!
//! QueryExecutor extracts structured CLI project data from RDF graph via SPARQL queries.
//! This implements Phase 3 of the RDF-to-CLI generator.

use anyhow::{Context as _, Result};
use oxigraph::sparql::{QueryResults, QuerySolution};
use oxigraph::store::Store;

use crate::rdf::types::*;

/// SPARQL query executor for extracting CLI project data from RDF graph
pub struct QueryExecutor<'a> {
    store: &'a Store,
}

impl<'a> QueryExecutor<'a> {
    /// Create a new QueryExecutor with a reference to the RDF store
    pub fn new(store: &'a Store) -> Self {
        Self { store }
    }

    /// Extract CliProject metadata from RDF graph
    ///
    /// Queries for project-level information including name, version, description,
    /// authors, edition, license, and workspace structure (2026 extension).
    pub fn extract_project(&self) -> Result<CliProject> {
        let query = r#"
            PREFIX cli: <http://ggen.dev/schema/cli#>
            PREFIX ex: <http://ggen.dev/projects/example-cli#>

            SELECT ?name ?version ?description ?authors ?edition ?license ?cliCrate ?domainCrate ?resolver
            WHERE {
                ?project a cli:CliProject ;
                    cli:hasName ?name ;
                    cli:hasVersion ?version ;
                    cli:hasDescription ?description ;
                    cli:hasAuthors ?authors ;
                    cli:hasEdition ?edition ;
                    cli:hasLicense ?license .
                OPTIONAL { ?project cli:hasCliCrate ?cliCrate }
                OPTIONAL { ?project cli:hasDomainCrate ?domainCrate }
                OPTIONAL { ?project cli:hasWorkspaceResolver ?resolver }
            }
            LIMIT 1
        "#;

        if let QueryResults::Solutions(mut solutions) = self.store.query(query)? {
            if let Some(solution) = solutions.next() {
                let solution = solution?;
                let name = get_string(&solution, "name")?;
                // Default CLI crate name if not specified
                let cli_crate = get_optional_string(&solution, "cliCrate")
                    .or_else(|| Some(format!("{}-cli", name)));
                // Default domain crate name if not specified
                let domain_crate = get_optional_string(&solution, "domainCrate")
                    .or_else(|| Some(format!("{}-core", name)));
                
                return Ok(CliProject {
                    name,
                    version: get_string(&solution, "version")?,
                    description: get_string(&solution, "description")?,
                    authors: vec![get_string(&solution, "authors")?],
                    edition: get_string(&solution, "edition")?,
                    license: get_string(&solution, "license")?,
                    nouns: vec![],       // Filled later
                    dependencies: vec![], // Filled later
                    cli_crate,
                    domain_crate,
                    resolver: get_optional_string(&solution, "resolver")
                        .unwrap_or_else(|| "2".to_string()),
                });
            }
        }

        anyhow::bail!("No project found in RDF graph")
    }

    /// Extract all Noun definitions from RDF graph
    ///
    /// A Noun represents a top-level command/resource in the CLI.
    /// For each Noun, extracts associated Verbs via extract_verbs_for_noun.
    pub fn extract_nouns(&self) -> Result<Vec<Noun>> {
        let query = r#"
            PREFIX cli: <http://ggen.dev/schema/cli#>
            PREFIX cnv: <http://ggen.dev/schema/clap-noun-verb#>
            PREFIX ex: <http://ggen.dev/projects/example-cli#>

            SELECT ?noun ?nounName ?nounDescription ?modulePath
            WHERE {
                ex:MyCliProject cli:hasNoun ?noun .
                ?noun cnv:nounName ?nounName ;
                      cnv:nounDescription ?nounDescription ;
                      cnv:nounModulePath ?modulePath .
            }
            ORDER BY ?nounName
        "#;

        let mut nouns = Vec::new();

        if let QueryResults::Solutions(solutions) = self.store.query(query)? {
            for solution in solutions {
                let solution = solution?;
                let noun_uri = get_uri(&solution, "noun")?;

                nouns.push(Noun {
                    name: get_string(&solution, "nounName")?,
                    description: get_string(&solution, "nounDescription")?,
                    module_path: get_string(&solution, "modulePath")?,
                    verbs: self.extract_verbs_for_noun(&noun_uri)?,
                });
            }
        }

        Ok(nouns)
    }

    /// Extract all Verbs for a specific Noun
    ///
    /// A Verb represents an action/operation on a Noun.
    /// For each Verb, extracts Arguments, Validations, and domain function references (2026 extension).
    fn extract_verbs_for_noun(&self, noun_uri: &str) -> Result<Vec<Verb>> {
        let query = format!(
            r#"
            PREFIX cnv: <http://ggen.dev/schema/clap-noun-verb#>

            SELECT ?verb ?verbName ?verbDescription ?alias ?executionLogic ?domainFunction ?domainModule
            WHERE {{
                <{noun_uri}> cnv:hasVerb ?verb .
                ?verb cnv:verbName ?verbName ;
                      cnv:verbDescription ?verbDescription .
                OPTIONAL {{ ?verb cnv:verbAlias ?alias }}
                OPTIONAL {{ ?verb cnv:executionLogic ?executionLogic }}
                OPTIONAL {{ ?verb cnv:domainFunction ?domainFunction }}
                OPTIONAL {{ ?verb cnv:domainModule ?domainModule }}
            }}
            ORDER BY ?verbName
        "#
        );

        let mut verbs = Vec::new();

        if let QueryResults::Solutions(solutions) = self.store.query(&query)? {
            for solution in solutions {
                let solution = solution?;
                let verb_uri = get_uri(&solution, "verb")?;

                verbs.push(Verb {
                    name: get_string(&solution, "verbName")?,
                    description: get_string(&solution, "verbDescription")?,
                    alias: get_optional_string(&solution, "alias"),
                    execution_logic: get_optional_string(&solution, "executionLogic"),
                    arguments: self.extract_arguments_for_verb(&verb_uri)?,
                    validations: self.extract_validations_for_verb(&verb_uri)?,
                    domain_function: get_optional_string(&solution, "domainFunction"),
                    domain_module: get_optional_string(&solution, "domainModule"),
                });
            }
        }

        Ok(verbs)
    }

    /// Extract all Arguments for a specific Verb
    ///
    /// Arguments include both positional and named (--long, -short) arguments,
    /// with types, defaults, and validation requirements.
    fn extract_arguments_for_verb(&self, verb_uri: &str) -> Result<Vec<Argument>> {
        let query = format!(
            r#"
            PREFIX cnv: <http://ggen.dev/schema/clap-noun-verb#>

            SELECT ?argName ?long ?short ?help ?required ?default ?valueName ?position ?typeName
            WHERE {{
                <{verb_uri}> cnv:hasArgument ?arg .
                ?arg cnv:argName ?argName ;
                     cnv:argHelp ?help .

                OPTIONAL {{ ?arg cnv:argLong ?long }}
                OPTIONAL {{ ?arg cnv:argShort ?short }}
                OPTIONAL {{ ?arg cnv:argRequired ?required }}
                OPTIONAL {{ ?arg cnv:argDefault ?default }}
                OPTIONAL {{ ?arg cnv:argValueName ?valueName }}
                OPTIONAL {{ ?arg cnv:argPosition ?position }}

                OPTIONAL {{
                    ?arg cnv:hasType ?type .
                    ?type cnv:typeName ?typeName
                }}
            }}
            ORDER BY ?position ?argName
        "#
        );

        let mut arguments = Vec::new();

        if let QueryResults::Solutions(solutions) = self.store.query(&query)? {
            for solution in solutions {
                let solution = solution?;

                arguments.push(Argument {
                    name: get_string(&solution, "argName")?,
                    long: get_optional_string(&solution, "long"),
                    short: get_optional_string(&solution, "short").and_then(|s| s.chars().next()),
                    help: get_string(&solution, "help")?,
                    required: get_optional_bool(&solution, "required").unwrap_or(false),
                    default: get_optional_string(&solution, "default"),
                    value_name: get_optional_string(&solution, "valueName"),
                    position: get_optional_int(&solution, "position"),
                    arg_type: ArgumentType {
                        name: get_optional_string(&solution, "typeName")
                            .unwrap_or_else(|| "String".to_string()),
                        parser: None,
                    },
                });
            }
        }

        Ok(arguments)
    }

    /// Extract all Validation rules for a specific Verb
    ///
    /// Validations define rules that arguments must satisfy (e.g., file_exists, regex patterns).
    fn extract_validations_for_verb(&self, verb_uri: &str) -> Result<Vec<Validation>> {
        let query = format!(
            r#"
            PREFIX cnv: <http://ggen.dev/schema/clap-noun-verb#>

            SELECT ?rule ?pattern ?message ?argName
            WHERE {{
                <{verb_uri}> cnv:hasValidation ?validation .
                ?validation cnv:validationRule ?rule ;
                            cnv:validationMessage ?message ;
                            cnv:validationArgName ?argName .
                OPTIONAL {{ ?validation cnv:validationPattern ?pattern }}
            }}
        "#
        );

        let mut validations = Vec::new();

        if let QueryResults::Solutions(solutions) = self.store.query(&query)? {
            for solution in solutions {
                let solution = solution?;

                validations.push(Validation {
                    rule: get_string(&solution, "rule")?,
                    pattern: get_optional_string(&solution, "pattern"),
                    message: get_string(&solution, "message")?,
                    arg_name: get_string(&solution, "argName")?,
                });
            }
        }

        Ok(validations)
    }

    /// Extract Cargo dependencies from RDF graph
    ///
    /// Dependencies include crate name, version, features, and optional flag.
    pub fn extract_dependencies(&self) -> Result<Vec<Dependency>> {
        let query = r#"
            PREFIX cli: <http://ggen.dev/schema/cli#>
            PREFIX cnv: <http://ggen.dev/schema/clap-noun-verb#>
            PREFIX ex: <http://ggen.dev/projects/example-cli#>

            SELECT ?depName ?version ?features ?optional
            WHERE {
                ex:MyCliProject cli:hasDependency ?dep .
                ?dep cnv:depName ?depName ;
                     cnv:depVersion ?version .
                OPTIONAL { ?dep cnv:depFeatures ?features }
                OPTIONAL { ?dep cnv:depOptional ?optional }
            }
        "#;

        let mut deps = Vec::new();

        if let QueryResults::Solutions(solutions) = self.store.query(query)? {
            for solution in solutions {
                let solution = solution?;
                deps.push(Dependency {
                    name: get_string(&solution, "depName")?,
                    version: get_string(&solution, "version")?,
                    features: get_optional_string(&solution, "features")
                        .map(|s| vec![s])
                        .unwrap_or_default(),
                    optional: get_optional_bool(&solution, "optional").unwrap_or(false),
                });
            }
        }

        Ok(deps)
    }
}

// ============================================================================
// Helper Functions
// ============================================================================

/// Extract required string value from SPARQL query solution
fn get_string(solution: &QuerySolution, var: &str) -> Result<String> {
    solution
        .get(var)
        .and_then(|term| {
            if let oxigraph::model::Term::Literal(lit) = term {
                Some(lit.value().to_string())
            } else {
                None
            }
        })
        .context(format!("Missing variable: {}", var))
}

/// Extract optional string value from SPARQL query solution
fn get_optional_string(solution: &QuerySolution, var: &str) -> Option<String> {
    solution.get(var).and_then(|term| {
        if let oxigraph::model::Term::Literal(lit) = term {
            Some(lit.value().to_string())
        } else {
            None
        }
    })
}

/// Extract URI from SPARQL query solution
fn get_uri(solution: &QuerySolution, var: &str) -> Result<String> {
    solution
        .get(var)
        .and_then(|term| {
            if let oxigraph::model::Term::NamedNode(node) = term {
                Some(node.as_str().to_string())
            } else {
                None
            }
        })
        .context(format!("Missing URI: {}", var))
}

/// Extract optional boolean value from SPARQL query solution
fn get_optional_bool(solution: &QuerySolution, var: &str) -> Option<bool> {
    solution.get(var).and_then(|term| {
        if let oxigraph::model::Term::Literal(lit) = term {
            lit.value().parse().ok()
        } else {
            None
        }
    })
}

/// Extract optional integer value from SPARQL query solution
fn get_optional_int(solution: &QuerySolution, var: &str) -> Option<usize> {
    solution.get(var).and_then(|term| {
        if let oxigraph::model::Term::Literal(lit) = term {
            lit.value().parse().ok()
        } else {
            None
        }
    })
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_query_executor_creation() {
        let store = Store::new().unwrap();
        let executor = QueryExecutor::new(&store);
        // Basic creation test - store reference is valid
        let _ = executor.store;
    }

    #[test]
    fn test_extract_project_empty_store() {
        let store = Store::new().unwrap();
        let executor = QueryExecutor::new(&store);
        let result = executor.extract_project();
        assert!(result.is_err(), "Should fail on empty store");
    }

    #[test]
    fn test_extract_nouns_empty_store() {
        let store = Store::new().unwrap();
        let executor = QueryExecutor::new(&store);
        let result = executor.extract_nouns();
        assert!(result.is_ok(), "Should return empty vec on empty store");
        assert_eq!(result.unwrap().len(), 0);
    }

    #[test]
    fn test_extract_dependencies_empty_store() {
        let store = Store::new().unwrap();
        let executor = QueryExecutor::new(&store);
        let result = executor.extract_dependencies();
        assert!(result.is_ok(), "Should return empty vec on empty store");
        assert_eq!(result.unwrap().len(), 0);
    }
}
