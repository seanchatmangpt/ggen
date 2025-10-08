# Multi-language CLI subcommand

```bash
rgen gen cli subcommand --vars cmd=hello summary="Print a greeting"
```

Produces, if templates exist:
```
src/cmds/hello.rs
commands/hello.py
commands/hello.sh
```

Same RDF + seed ⇒ byte-identical outputs.
