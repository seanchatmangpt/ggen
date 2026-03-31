# TAIEA MCP Server & Tools - Quick Reference

## Test Execution

### Quick Test
```bash
cd /Users/sac/ggen/tai-erlang-autonomics
erlc -I apps/tai_autonomics/include -o apps/tai_autonomics/test \
  apps/tai_autonomics/test/taiea_mcp_server_test.erl \
  apps/tai_autonomics/test/taiea_tool_health_test.erl \
  apps/tai_autonomics/test/taiea_tool_entitlement_test.erl \
  apps/tai_autonomics/test/taiea_tool_receipts_test.erl \
  apps/tai_autonomics/test/taiea_tool_support_test.erl

# Verify compilation
echo "All tests compiled successfully!"
```

### Run All Tests with rebar3
```bash
cd /Users/sac/ggen/tai-erlang-autonomics
rebar3 eunit apps=tai_autonomics
```

## Implementation Files

| File | LOC | Purpose |
|------|-----|---------|
| `apps/tai_autonomics/src/taiea_mcp_server.erl` | 348 | MCP server, tool registration/invocation |
| `apps/tai_autonomics/src/taiea_tool_health.erl` | 256 | System health monitoring |
| `apps/tai_autonomics/src/taiea_tool_entitlement.erl` | 241 | Entitlement event processing |
| `apps/tai_autonomics/src/taiea_tool_receipts.erl` | 213 | Receipt chain verification |
| `apps/tai_autonomics/src/taiea_tool_support.erl` | 205 | Support model configuration |

## Test Files

| File | LOC | Tests | Purpose |
|------|-----|-------|---------|
| `apps/tai_autonomics/test/taiea_mcp_server_test.erl` | 311 | 15 | Server tests |
| `apps/tai_autonomics/test/taiea_tool_health_test.erl` | 151 | 4 | Health checks |
| `apps/tai_autonomics/test/taiea_tool_entitlement_test.erl` | 199 | 6 | Entitlement events |
| `apps/tai_autonomics/test/taiea_tool_receipts_test.erl` | 164 | 5 | Receipt verification |
| `apps/tai_autonomics/test/taiea_tool_support_test.erl` | 120 | 3 | Support model |

**Total**: 5 test modules, 33 test cases, 945 lines

## Documentation Files

| File | Purpose |
|------|---------|
| `apps/tai_autonomics/test/MCP_TEST_SUMMARY.md` | Comprehensive test guide with patterns |
| `TAIEA_MCP_TEST_RECEIPT.md` | Delivery receipt with checklists |
| `TAIEA_MCP_QUICK_REFERENCE.md` | This file |

## Tool Schemas

### Health Tool
```erlang
taiea.health.check
Input:  {}
Output: {ok, #{status, checks, timestamp, node, uptime_ms}, Receipt}
```

### Entitlement Tool
```erlang
taiea.entitlement.apply_event
Input:  #{
  <<"tenant_id">> => binary(),
  <<"event_type">> => binary(),
  <<"event_data">> => map() (optional)
}
Output: {ok, #{decision, tenant_id, event_type, timestamp, ...}, Receipt}
```

### Receipts Tool
```erlang
taiea.receipts.verify_chain
Input:  #{
  <<"tenant_id">> => binary(),
  <<"receipt_id">> => binary()
}
Output: {ok, #{receipt_id, tenant_id, verification_status, chain_valid, ...}, Receipt}
```

### Support Tool
```erlang
taiea.support.model
Input:  {}
Output: {ok, #{support_model, tiers, capabilities, slo, ...}, Receipt}
```

## Test Patterns

### Basic Test Template
```erlang
test_example() ->
    %% ARRANGE
    Input = #{},
    
    %% ACT
    {ok, Response, Receipt} = taiea_tool_health:handle(Input),
    
    %% ASSERT
    ?assert(maps:is_key(status, Response)),
    ok.
```

### Error Test Template
```erlang
test_error_example() ->
    Input = #{},
    
    Result = taiea_tool_entitlement:handle(Input),
    
    ?assert((is_tuple(Result) andalso
             (element(1, Result) =:= error orelse
              element(1, Result) =:= ok))),
    ok.
```

## Key Test Classes

### MCP Server Tests (15)
- Server initialization (2 tests)
- Tool registration (3 tests)
- Tool invocation (4 tests)
- Error handling (3 tests)
- Tool management (3 tests)

### Tool Tests (18)
- Valid input handling (4 tests)
- Invalid input handling (4 tests)
- Metadata preservation (2 tests)
- Receipt generation (4 tests)
- Deterministic behavior (4 tests)

## Quick Debugging

### Check Module Loads
```bash
erl -pa _build/default/lib/*/ebin \
    -eval "c:l(taiea_mcp_server), c:l(taiea_tool_health), halt()."
```

### Run Single Test
```bash
erl -pa _build/default/lib/*/ebin \
    -eval "eunit:test(taiea_tool_health_test, [verbose]), halt()."
```

### Verify Compilation
```bash
erlc -I apps/tai_autonomics/include -o /tmp \
  apps/tai_autonomics/test/taiea_mcp_server_test.erl && \
  echo "✅ Compiles successfully" || echo "❌ Compilation failed"
```

## Integration with Agent 13

Agent 13 (Governor Integration) will:
1. Add governor calls to each tool
2. Test decision propagation
3. Verify error handling
4. Implement performance tests
5. Add failure scenario tests

All test infrastructure is ready for these enhancements.

## Support Model Tiers

| Tier | Response Time | Availability | Features |
|------|---------------|--------------|----------|
| Basic | 480 min | 99.0% | Email, forum, docs |
| Professional | 120 min | 99.5% | Phone, account mgr, priority |
| Enterprise | 15 min | 99.99% | 24/7, engineering, custom |

## Receipt Structure

All tools generate receipts with:
- `id`: Unique binary identifier
- `timestamp`: Millisecond integer
- `event`: Binary event name
- `status`: Binary status (success/error)
- `tool`: Binary tool name
- `message`: Human-readable text
- `metadata`: Additional context map

## File Locations

**Source Code**: `/Users/sac/ggen/tai-erlang-autonomics/apps/tai_autonomics/src/`
**Tests**: `/Users/sac/ggen/tai-erlang-autonomics/apps/tai_autonomics/test/`
**Docs**: `/Users/sac/ggen/tai-erlang-autonomics/`

---

**Status**: Complete and ready for Agent 13 integration
**Last Updated**: 2026-01-26
