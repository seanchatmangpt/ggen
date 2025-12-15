# SPARQL Queries for CLI Generation

Complete SPARQL queries for extracting project structure from TTL files.

## 1. Extract Complete Project Metadata

```sparql
PREFIX cli: <http://ggen.dev/schema/cli#>
PREFIX cnv: <http://ggen.dev/schema/clap-noun-verb#>
PREFIX ex: <http://ggen.dev/projects/example-cli#>

SELECT ?name ?version ?description ?authors ?edition ?license
WHERE {
  ex:MyCliProject a cli:CliProject ;
                  cli:hasName ?name ;
                  cli:hasVersion ?version ;
                  cli:hasDescription ?description ;
                  cli:hasAuthors ?authors ;
                  cli:hasEdition ?edition ;
                  cli:hasLicense ?license .
}
```

**Output:**
```json
{
  "name": "my-cli",
  "version": "0.1.0",
  "description": "A CLI tool for managing templates and projects",
  "authors": "Your Name <you@example.com>",
  "edition": "2021",
  "license": "MIT"
}
```

## 2. Extract All Nouns

```sparql
PREFIX cli: <http://ggen.dev/schema/cli#>
PREFIX cnv: <http://ggen.dev/schema/clap-noun-verb#>
PREFIX ex: <http://ggen.dev/projects/example-cli#>

SELECT ?noun ?nounName ?nounDescription ?modulePath
WHERE {
  ex:MyCliProject cli:hasNoun ?noun .
  ?noun a cnv:Noun ;
        cnv:nounName ?nounName ;
        cnv:nounDescription ?nounDescription ;
        cnv:nounModulePath ?modulePath .
}
ORDER BY ?nounName
```

**Output:**
```json
[
  {
    "noun": "ex:ProjectNoun",
    "nounName": "project",
    "nounDescription": "Manage projects",
    "modulePath": "cmds::project"
  },
  {
    "noun": "ex:TemplateNoun",
    "nounName": "template",
    "nounDescription": "Manage project templates",
    "modulePath": "cmds::template"
  }
]
```

## 3. Extract Verbs for Each Noun

```sparql
PREFIX cnv: <http://ggen.dev/schema/clap-noun-verb#>
PREFIX ex: <http://ggen.dev/projects/example-cli#>

SELECT ?nounName ?verb ?verbName ?verbDescription ?alias
WHERE {
  ?noun cnv:nounName ?nounName ;
        cnv:hasVerb ?verb .
  ?verb cnv:verbName ?verbName ;
        cnv:verbDescription ?verbDescription .
  OPTIONAL { ?verb cnv:verbAlias ?alias }
}
ORDER BY ?nounName ?verbName
```

**Output:**
```json
[
  {
    "nounName": "project",
    "verb": "ex:ProjectBuild",
    "verbName": "build",
    "verbDescription": "Build the project",
    "alias": null
  },
  {
    "nounName": "project",
    "verb": "ex:ProjectInit",
    "verbName": "init",
    "verbDescription": "Initialize a new project",
    "alias": null
  },
  {
    "nounName": "template",
    "verb": "ex:TemplateGenerate",
    "verbName": "generate",
    "verbDescription": "Generate a new project from template",
    "alias": "gen"
  }
]
```

## 4. Extract Arguments for a Specific Verb

```sparql
PREFIX cnv: <http://ggen.dev/schema/clap-noun-verb#>
PREFIX ex: <http://ggen.dev/projects/example-cli#>

SELECT ?argName ?long ?short ?help ?required ?default ?valueName ?position ?typeName
WHERE {
  ex:TemplateGenerate cnv:hasArgument ?arg .
  ?arg cnv:argName ?argName ;
       cnv:argHelp ?help .

  # Optional fields
  OPTIONAL { ?arg cnv:argLong ?long }
  OPTIONAL { ?arg cnv:argShort ?short }
  OPTIONAL { ?arg cnv:argRequired ?required }
  OPTIONAL { ?arg cnv:argDefault ?default }
  OPTIONAL { ?arg cnv:argValueName ?valueName }
  OPTIONAL { ?arg cnv:argPosition ?position }

  # Type information
  OPTIONAL {
    ?arg cnv:hasType ?type .
    ?type cnv:typeName ?typeName
  }
}
ORDER BY ?position ?argName
```

**Output:**
```json
[
  {
    "argName": "template_name",
    "long": null,
    "short": null,
    "help": "Name of the template to use",
    "required": true,
    "default": null,
    "valueName": "TEMPLATE",
    "position": 0,
    "typeName": "String"
  },
  {
    "argName": "output_path",
    "long": "output",
    "short": "o",
    "help": "Output directory path",
    "required": false,
    "default": ".",
    "valueName": "PATH",
    "position": null,
    "typeName": "PathBuf"
  },
  {
    "argName": "force",
    "long": "force",
    "short": "f",
    "help": "Overwrite existing files",
    "required": false,
    "default": "false",
    "valueName": null,
    "position": null,
    "typeName": "bool"
  }
]
```

## 5. Extract All Arguments with Types

```sparql
PREFIX cnv: <http://ggen.dev/schema/clap-noun-verb#>

SELECT ?nounName ?verbName ?argName ?typeName ?required ?default
WHERE {
  ?noun cnv:nounName ?nounName ;
        cnv:hasVerb ?verb .
  ?verb cnv:verbName ?verbName ;
        cnv:hasArgument ?arg .
  ?arg cnv:argName ?argName .

  OPTIONAL { ?arg cnv:argRequired ?required }
  OPTIONAL { ?arg cnv:argDefault ?default }

  OPTIONAL {
    ?arg cnv:hasType ?type .
    ?type cnv:typeName ?typeName
  }
}
ORDER BY ?nounName ?verbName ?argName
```

## 6. Extract Validation Rules

```sparql
PREFIX cnv: <http://ggen.dev/schema/clap-noun-verb#>
PREFIX ex: <http://ggen.dev/projects/example-cli#>

SELECT ?verbName ?argName ?rule ?pattern ?message
WHERE {
  ?verb cnv:verbName ?verbName ;
        cnv:hasArgument ?arg ;
        cnv:hasValidation ?validation .

  ?arg cnv:argName ?argName ;
       cnv:hasValidation ?validation .

  ?validation cnv:validationRule ?rule .

  OPTIONAL { ?validation cnv:validationPattern ?pattern }
  OPTIONAL { ?validation cnv:validationMessage ?message }
}
```

**Output:**
```json
[
  {
    "verbName": "lint",
    "argName": "template_path",
    "rule": "file_exists",
    "pattern": null,
    "message": "Path does not exist"
  }
]
```

## 7. Extract Dependencies

```sparql
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
ORDER BY ?depName
```

**Output:**
```json
[
  {
    "depName": "anyhow",
    "version": "1.0",
    "features": null,
    "optional": null
  },
  {
    "depName": "clap",
    "version": "4.0",
    "features": "derive",
    "optional": null
  },
  {
    "depName": "serde_json",
    "version": "1.0",
    "features": null,
    "optional": null
  }
]
```

## 8. Extract Execution Logic

```sparql
PREFIX cnv: <http://ggen.dev/schema/clap-noun-verb#>

SELECT ?nounName ?verbName ?executionLogic
WHERE {
  ?noun cnv:nounName ?nounName ;
        cnv:hasVerb ?verb .
  ?verb cnv:verbName ?verbName .

  OPTIONAL { ?verb cnv:executionLogic ?executionLogic }
}
```

## 9. Complete Hierarchical Query (Noun > Verb > Args)

```sparql
PREFIX cli: <http://ggen.dev/schema/cli#>
PREFIX cnv: <http://ggen.dev/schema/clap-noun-verb#>
PREFIX ex: <http://ggen.dev/projects/example-cli#>

SELECT ?nounName ?verbName ?verbDesc ?argName ?argLong ?argShort ?argHelp ?typeName
WHERE {
  ex:MyCliProject cli:hasNoun ?noun .
  ?noun cnv:nounName ?nounName ;
        cnv:hasVerb ?verb .
  ?verb cnv:verbName ?verbName ;
        cnv:verbDescription ?verbDesc ;
        cnv:hasArgument ?arg .
  ?arg cnv:argName ?argName ;
       cnv:argHelp ?argHelp .

  OPTIONAL { ?arg cnv:argLong ?argLong }
  OPTIONAL { ?arg cnv:argShort ?argShort }

  OPTIONAL {
    ?arg cnv:hasType ?type .
    ?type cnv:typeName ?typeName
  }
}
ORDER BY ?nounName ?verbName ?argName
```

## 10. Generate Rust Struct Field from Arguments

```sparql
PREFIX cnv: <http://ggen.dev/schema/clap-noun-verb#>
PREFIX ex: <http://ggen.dev/projects/example-cli#>

SELECT ?argName ?typeName ?required ?default
WHERE {
  ex:TemplateGenerate cnv:hasArgument ?arg .
  ?arg cnv:argName ?argName ;
       cnv:hasType ?type .
  ?type cnv:typeName ?typeName .

  OPTIONAL { ?arg cnv:argRequired ?required }
  OPTIONAL { ?arg cnv:argDefault ?default }
}
```

**Use in template:**
```rust
#[derive(Args)]
pub struct GenerateArgs {
    {% for arg in args %}
    pub {{ arg.argName }}: {{ arg.typeName }},
    {% endfor %}
}
```

## 11. Extract Only Positional Arguments

```sparql
PREFIX cnv: <http://ggen.dev/schema/clap-noun-verb#>

SELECT ?argName ?position ?typeName ?valueName
WHERE {
  ?verb cnv:hasArgument ?arg .
  ?arg cnv:argName ?argName ;
       cnv:argPosition ?position ;
       cnv:argValueName ?valueName ;
       cnv:hasType ?type .
  ?type cnv:typeName ?typeName .
}
ORDER BY ?position
```

## 12. Extract Only Flag Arguments

```sparql
PREFIX cnv: <http://ggen.dev/schema/clap-noun-verb#>

SELECT ?argName ?long ?short ?typeName ?default
WHERE {
  ?verb cnv:hasArgument ?arg .
  ?arg cnv:argName ?argName ;
       cnv:argLong ?long ;
       cnv:hasType ?type .
  ?type cnv:typeName ?typeName .

  OPTIONAL { ?arg cnv:argShort ?short }
  OPTIONAL { ?arg cnv:argDefault ?default }

  FILTER NOT EXISTS { ?arg cnv:argPosition ?position }
}
ORDER BY ?argName
```

## 13. Count Commands per Noun

```sparql
PREFIX cli: <http://ggen.dev/schema/cli#>
PREFIX cnv: <http://ggen.dev/schema/clap-noun-verb#>

SELECT ?nounName (COUNT(?verb) as ?verbCount)
WHERE {
  ?project cli:hasNoun ?noun .
  ?noun cnv:nounName ?nounName ;
        cnv:hasVerb ?verb .
}
GROUP BY ?nounName
ORDER BY DESC(?verbCount)
```

## 14. Find All Required Arguments

```sparql
PREFIX cnv: <http://ggen.dev/schema/clap-noun-verb#>

SELECT ?nounName ?verbName ?argName ?help
WHERE {
  ?noun cnv:nounName ?nounName ;
        cnv:hasVerb ?verb .
  ?verb cnv:verbName ?verbName ;
        cnv:hasArgument ?arg .
  ?arg cnv:argName ?argName ;
       cnv:argHelp ?help ;
       cnv:argRequired true .
}
ORDER BY ?nounName ?verbName ?argName
```

## 15. Extract File Tree Generation Data

```sparql
PREFIX cli: <http://ggen.dev/schema/cli#>
PREFIX cnv: <http://ggen.dev/schema/clap-noun-verb#>

CONSTRUCT {
  ?project cli:hasName ?name ;
           cli:hasNoun ?noun .
  ?noun cnv:nounName ?nounName ;
        cnv:hasVerb ?verb .
  ?verb cnv:verbName ?verbName .
}
WHERE {
  ?project a cli:CliProject ;
           cli:hasName ?name ;
           cli:hasNoun ?noun .
  ?noun cnv:nounName ?nounName ;
        cnv:hasVerb ?verb .
  ?verb cnv:verbName ?verbName .
}
```

## Using Queries in Rust

```rust
use oxigraph::store::Store;
use oxigraph::sparql::QueryResults;

pub fn extract_nouns(store: &Store) -> Result<Vec<NounData>> {
    let query = r#"
        PREFIX cli: <http://ggen.dev/schema/cli#>
        PREFIX cnv: <http://ggen.dev/schema/clap-noun-verb#>

        SELECT ?nounName ?nounDescription
        WHERE {
            ?project cli:hasNoun ?noun .
            ?noun cnv:nounName ?nounName ;
                  cnv:nounDescription ?nounDescription .
        }
    "#;

    let mut nouns = Vec::new();

    if let QueryResults::Solutions(solutions) = store.query(query)? {
        for solution in solutions {
            let solution = solution?;
            let name = solution.get("nounName")
                .and_then(|t| t.as_str())
                .unwrap();
            let desc = solution.get("nounDescription")
                .and_then(|t| t.as_str())
                .unwrap();

            nouns.push(NounData {
                name: name.to_string(),
                description: desc.to_string(),
            });
        }
    }

    Ok(nouns)
}
```

## Template Context Builder

```rust
pub fn build_template_context(store: &Store) -> Result<Context> {
    let mut context = Context::new();

    // Extract project metadata
    let project = extract_project_metadata(store)?;
    context.insert("project", &project);

    // Extract nouns with verbs and arguments
    let nouns = extract_nouns_with_verbs(store)?;
    context.insert("nouns", &nouns);

    // Extract dependencies
    let deps = extract_dependencies(store)?;
    context.insert("dependencies", &deps);

    Ok(context)
}
```
