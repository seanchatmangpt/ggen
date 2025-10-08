# Quickstart

Init folders:
```bash
mkdir -p templates/cli/subcommand
```

Create `templates/cli/subcommand/rust.tmpl`:

```yaml
---
to: src/cmds/{{ slug }}.rs
vars: { cmd: hello, summary: "Print a greeting", seed: cosmos }
rdf:
  inline:
    - mediaType: text/turtle
      text: |
        @prefix cli: <urn:rgen:cli#> .
        [] a cli:Command ; cli:slug "{{ cmd }}" ; cli:summary "{{ summary }}" .
sparql:
  vars:
    - name: slug
      query: |
        PREFIX cli: <urn:rgen:cli#>
        SELECT ?slug WHERE { ?c a cli:Command ; cli:slug ?slug } LIMIT 1
determinism: { seed: "{{ seed }}" }
---
pub fn {{ slug }}(name:&str){ println!("hello {}", name); }
```

Generate:

```bash
rgen gen cli subcommand --vars cmd=hello summary="Print a greeting"
```

Result:

```
src/cmds/hello.rs
```
