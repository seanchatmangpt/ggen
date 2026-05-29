#!/usr/bin/env python3
"""Instant ggen-lsp-mcp wire smoke check (runs in seconds).

Spawns the real `ggen-lsp-mcp` binary and drives the MCP stdio handshake an MCP client
(Claude Code via .mcp.json) performs: initialize -> tools/list -> tools/call. Prints the
server identity, the 3 tools, and the live E0011 route. Exit 0 iff E0011 observed.

rmcp's stdio transport is NEWLINE-delimited JSON-RPC (one object per line).

Usage:  cargo build -p ggen-lsp-mcp && python3 scripts/mcp-smoke.py
Binary resolution: $GGEN_LSP_MCP_BIN, else target/debug/ggen-lsp-mcp, else PATH.
"""
import json, os, signal, subprocess, sys, tempfile

BIN = os.environ.get("GGEN_LSP_MCP_BIN") or (
    "target/debug/ggen-lsp-mcp" if os.path.exists("target/debug/ggen-lsp-mcp") else "ggen-lsp-mcp")
BROKEN = "CONSTRUCT { ?s ?p ?o } WHERE { ?s ?p ?o }\n"


def main():
    signal.alarm(20)
    tmp = tempfile.mkdtemp()
    p = subprocess.Popen([BIN], stdin=subprocess.PIPE, stdout=subprocess.PIPE,
                         stderr=subprocess.DEVNULL, cwd=tmp, text=True)
    stdin, stdout = p.stdin, p.stdout
    assert stdin and stdout  # narrow Optional pipes + guard

    def send(m):
        stdin.write(json.dumps(m) + "\n"); stdin.flush()

    def recv_id(want):
        for _ in range(100):
            line = stdout.readline()
            if not line:
                return None
            line = line.strip()
            if not line:
                continue
            try:
                m = json.loads(line)
            except ValueError:
                continue
            if m.get("id") == want:
                return m

    seen_e0011 = False
    try:
        send({"jsonrpc": "2.0", "id": 1, "method": "initialize",
              "params": {"protocolVersion": "2024-11-05", "capabilities": {},
                         "clientInfo": {"name": "mcp-smoke", "version": "1.0"}}})
        init = recv_id(1)
        assert init, "no initialize response from ggen-lsp-mcp"
        si = init["result"]["serverInfo"]
        print(f"MCP server: {si.get('name')} {si.get('version')}")
        send({"jsonrpc": "2.0", "method": "notifications/initialized"})

        send({"jsonrpc": "2.0", "id": 2, "method": "tools/list"})
        listed = recv_id(2)
        assert listed, "no tools/list response"
        tools = [t["name"] for t in listed["result"]["tools"]]
        print("tools:", ", ".join(tools))

        send({"jsonrpc": "2.0", "id": 3, "method": "tools/call",
              "params": {"name": "ggen.lsp.repair_route",
                         "arguments": {"file_path": "q.rq", "file_content": BROKEN}}})
        call = recv_id(3)
        assert call, "no tools/call response"
        doc = json.loads(call["result"]["content"][0]["text"])
        env = (doc.get("envelopes") or [{}])[0]
        print(f"  is_law_surface: {doc.get('is_law_surface')}")
        print(f"  route: {env.get('route_id')} (for {env.get('diagnostic_code')})")
        seen_e0011 = env.get("diagnostic_code") == "E0011"
    finally:
        p.kill()

    print("\nE0011 observed:", seen_e0011)
    sys.exit(0 if seen_e0011 else 1)


if __name__ == "__main__":
    main()
