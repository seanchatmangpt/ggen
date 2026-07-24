# Architecture

## Law-state flow

```text
parse request
  -> validate UIMessage[]
  -> instantiate admitted ToolLoopAgent
  -> model selects text or tool call
  -> approval gate for mutating tools
  -> ToolBroker resolves finite handlerKey
  -> adapter actuates or refuses
  -> tool result and request-id receipt stream to UI
```

## Ownership

- Ontology owns selectable models, agent instructions, tools, handlers, approval policy, versions, and operational limits.
- Pack templates own the projection from admitted graph state to source files.
- Registry locks own external shadcn/ui and AI Elements acquisition.
- Application adapters own domain behavior behind `ToolBroker`.
- Tests and receipts own standing claims.

## Exclusions

- No arbitrary JavaScript is stored in RDF.
- No tool directly actuates outside `ToolBroker`.
- No mutating tool is admitted without approval.
- No `generated/` directory is used.
- No missing adapter is interpreted as success.
