#!/usr/bin/env python3
"""Instant ggen-lsp wire smoke check (runs in seconds).

Spawns the real `ggen-lsp` binary, performs the LSP `initialize` handshake Claude Code
performs, opens a broken SPARQL law surface, and prints the advertised capabilities + the
live E0011 diagnostic + repair route. Exit 0 iff E0011 was observed (so it doubles as a
CI canary).

Usage:  cargo build -p ggen-lsp && python3 scripts/lsp-smoke.py
Binary resolution: $GGEN_LSP_BIN, else target/debug/ggen-lsp, else `ggen-lsp` on PATH.
"""
import json, os, signal, subprocess, sys, tempfile

BIN = os.environ.get("GGEN_LSP_BIN") or (
    "target/debug/ggen-lsp" if os.path.exists("target/debug/ggen-lsp") else "ggen-lsp")
BROKEN = "CONSTRUCT { ?s ?p ?o } WHERE { ?s ?p ?o }\n"
URI = "file:///smoke/q.rq"


def frame(msg):
    b = json.dumps(msg).encode()
    return f"Content-Length: {len(b)}\r\n\r\n".encode() + b


def read_frame(f):
    n = 0
    while True:
        line = f.readline()
        if not line:
            return None
        line = line.strip()
        if line == b"":
            break
        if line.lower().startswith(b"content-length:"):
            n = int(line.split(b":", 1)[1].strip())
    return json.loads(f.read(n))


def main():
    signal.alarm(20)  # hard timeout so the smoke check never hangs CI
    tmp = tempfile.mkdtemp()
    p = subprocess.Popen([BIN], stdin=subprocess.PIPE, stdout=subprocess.PIPE,
                         stderr=subprocess.DEVNULL, cwd=tmp)
    stdin, stdout = p.stdin, p.stdout
    assert stdin and stdout  # narrow Optional pipes + guard

    def send(m):
        stdin.write(frame(m)); stdin.flush()

    seen_e0011 = False
    diag_range = None
    try:
        send({"jsonrpc": "2.0", "id": 1, "method": "initialize",
              "params": {"processId": None, "rootUri": None, "capabilities": {}}})
        init = read_frame(stdout)
        assert init, "no initialize response from ggen-lsp"
        caps = init["result"]["capabilities"]
        print("ggen-lsp", init["result"]["serverInfo"].get("version", "?"), "capabilities:")
        print("  " + ", ".join(sorted(k for k, v in caps.items() if v)))
        print("  call_hierarchy advertised:", "callHierarchyProvider" in caps,
              "| type_hierarchy advertised:", "typeHierarchyProvider" in caps)

        send({"jsonrpc": "2.0", "method": "initialized", "params": {}})
        send({"jsonrpc": "2.0", "method": "textDocument/didOpen",
              "params": {"textDocument": {"uri": URI, "languageId": "sparql", "version": 1, "text": BROKEN}}})

        for _ in range(50):
            f = read_frame(stdout)
            if f is None:
                break
            if f.get("method") == "textDocument/publishDiagnostics" and f["params"]["uri"] == URI:
                for d in f["params"]["diagnostics"]:
                    print(f"  diagnostic {d.get('code')}: {d.get('message')}")
                    if d.get("code") == "E0011":
                        seen_e0011 = True
                        diag_range = d["range"]
                break

        if diag_range:
            send({"jsonrpc": "2.0", "id": 2, "method": "textDocument/codeAction",
                  "params": {"textDocument": {"uri": URI}, "range": diag_range,
                             "context": {"diagnostics": [{"code": "E0011", "range": diag_range,
                                                          "message": "", "severity": 2}]}}})
            for _ in range(50):
                f = read_frame(stdout)
                if f is None:
                    break
                if f.get("id") == 2:
                    for a in (f.get("result") or []):
                        data = a.get("data") or {}
                        print(f"  repair route: {data.get('route_id')} (for {data.get('diagnostic_code')})")
                    break
        send({"jsonrpc": "2.0", "id": 3, "method": "shutdown", "params": None})
    finally:
        try:
            send({"jsonrpc": "2.0", "method": "exit", "params": None})
        except Exception:
            pass
        p.kill()

    print("\nE0011 observed:", seen_e0011)
    sys.exit(0 if seen_e0011 else 1)


if __name__ == "__main__":
    main()
