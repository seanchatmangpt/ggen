<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->
**Table of Contents**

- [Recipe: Your First Template](#recipe-your-first-template)
  - [What You'll Build](#what-youll-build)
  - [Prerequisites](#prerequisites)
  - [The Recipe](#the-recipe)
    - [Step 1: Create Your Graph Data](#step-1-create-your-graph-data)
    - [Step 2: Create Your Template](#step-2-create-your-template)
    - [Step 3: Generate Code](#step-3-generate-code)
    - [Step 4: Verify the Output](#step-4-verify-the-output)
    - [Step 5: Test It (Optional)](#step-5-test-it-optional)
  - [What Just Happened?](#what-just-happened)
  - [The Hidden Magic](#the-hidden-magic)
  - [Common Mistakes](#common-mistakes)
  - [Next Steps](#next-steps)
  - [Related Patterns](#related-patterns)
  - [Troubleshooting](#troubleshooting)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

# Recipe: Your First Template

**Time:** 5 minutes
**Difficulty:** Beginner
**Patterns:** Basic templating, variable binding

## What You'll Build

A simple Rust function generator that creates greeting functions from graph data. This is your "Hello World" for GGen—a complete working example to verify your setup.

## Prerequisites

- GGen installed (`cargo install ggen-cli`)
- Basic understanding of templates (any language)

## The Recipe

### Step 1: Create Your Graph Data

Create `data/greeter.ttl`:

```turtle
@prefix : <http://example.org/> .

:hello_function
  :function_name "greet" ;
  :parameter_name "name" ;
  :parameter_type "str" ;
  :greeting_message "Hello" .

:goodbye_function
  :function_name "farewell" ;
  :parameter_name "person" ;
  :parameter_type "str" ;
  :greeting_message "Goodbye" .
```

**What's happening:** Each subject (`:hello_function`, `:goodbye_function`) represents one function we'll generate. The predicates are variables we'll use in our template.

### Step 2: Create Your Template

Create `templates/greeter.tmpl`:

```handlebars
{{#each results}}
pub fn {{function_name}}({{parameter_name}}: &{{parameter_type}}) -> String {
    format!("{{greeting_message}}, {}!", {{parameter_name}})
}

{{/each}}
```

**What's happening:**
- `{{#each results}}` iterates over each subject from the graph
- `{{function_name}}`, `{{parameter_name}}`, etc. are replaced with values from predicates
- Each iteration produces one complete function

### Step 3: Generate Code

```bash
ggen exec \
  --template templates/greeter.tmpl \
  --data data/greeter.ttl \
  --output src/greeter.rs
```

### Step 4: Verify the Output

Check `src/greeter.rs`:

```rust
pub fn greet(name: &str) -> String {
    format!("Hello, {}!", name)
}

pub fn farewell(person: &str) -> String {
    format!("Goodbye, {}!", person)
}
```

### Step 5: Test It (Optional)

Create `src/main.rs`:

```rust
mod greeter;

fn main() {
    println!("{}", greeter::greet("Alice"));
    println!("{}", greeter::farewell("Bob"));
}
```

Run it:

```bash
cargo run
# Output:
# Hello, Alice!
# Goodbye, Bob!
```

## What Just Happened?

1. **Graph Data**: You defined structured data (functions) in Turtle RDF format
2. **Template**: You created a Handlebars template with variable placeholders
3. **Generation**: GGen executed a SPARQL query behind the scenes to bind variables
4. **Output**: Two complete Rust functions were generated

## The Hidden Magic

GGen automatically ran this SPARQL query:

```sparql
SELECT DISTINCT ?function_name ?parameter_name ?parameter_type ?greeting_message
WHERE {
  ?subject :function_name ?function_name ;
           :parameter_name ?parameter_name ;
           :parameter_type ?parameter_type ;
           :greeting_message ?greeting_message .
}
```

You didn't write it—GGen inferred it from your template variables!

## Common Mistakes

1. **Missing predicates**: If a subject doesn't have all predicates, it won't appear in results
   - Fix: Ensure all subjects have complete data

2. **Typos in variable names**: `{{functon_name}}` vs `{{function_name}}`
   - Fix: Template variables must exactly match predicate names

3. **Wrong file paths**: Template or data file not found
   - Fix: Use absolute paths or verify current directory

## Next Steps

- **Add more functions**: Add more subjects to `greeter.ttl`
- **Add complexity**: Include return types, multiple parameters
- **Try patterns**: Move to [CLI Command Scaffold](./cli_command_scaffold.md) for freeze blocks

## Related Patterns

- [Variable Binding](../patterns/variable_binding.md) - Deep dive into how variables work
- [Basic Generation](../patterns/basic_generation.md) - Understanding the generation pipeline

## Troubleshooting

**No output generated?**
```bash
# Add --verbose to see what's happening
ggen exec --verbose \
  --template templates/greeter.tmpl \
  --data data/greeter.ttl
```

**Graph parsing errors?**
```bash
# Validate your Turtle syntax
ggen validate --data data/greeter.ttl
```

---

**Success checkpoint:** You should have working Rust functions that compile and run. If not, check the troubleshooting section or compare your files character-by-character with the examples above.
