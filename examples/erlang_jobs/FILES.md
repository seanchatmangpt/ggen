# Erlang Jobs Example - File Inventory

## RDF Ontology
- `.specify/specs/001-job-processor/ontology.ttl` - Complete Erlang domain ontology (235 lines, 50+ triples)

## Tera Templates
- `templates/erlang/gen_server.erl.tera` - GenServer implementation template
- `templates/erlang/supervisor.erl.tera` - Supervisor implementation template
- `templates/erlang/application.erl.tera` - OTP application template
- `templates/rebar3/rebar.config.tera` - Rebar3 build configuration template
- `templates/rebar3/app.src.tera` - Application resource file template

## Configuration
- `ggen.toml` - Generation manifest with SPARQL rules
- `generate.sh` - Automated code generation script

## Documentation
- `README.md` - Comprehensive usage guide (280 lines)

## Output Directory
- `generated/` - Generated Erlang code (created by ggen sync)

## Usage

1. Generate code: `./generate.sh`
2. Build: `cd generated && rebar3 compile`
3. Test: `rebar3 eunit`
4. Run: `rebar3 shell`
