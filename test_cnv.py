import subprocess
import json

p = subprocess.Popen(["target/debug/clap-noun-verb-pack-lsp"], stdin=subprocess.PIPE, stdout=subprocess.PIPE)

def send(msg):
    body = json.dumps(msg).encode('utf-8')
    header = f"Content-Length: {len(body)}\r\n\r\n".encode('utf-8')
    p.stdin.write(header)
    p.stdin.write(body)
    p.stdin.flush()

send({
    "jsonrpc": "2.0",
    "id": 1,
    "method": "initialize",
    "params": {"processId": None, "rootUri": None, "capabilities": {}}
})
send({
    "jsonrpc": "2.0",
    "method": "initialized",
    "params": {}
})
send({
    "jsonrpc": "2.0",
    "method": "textDocument/didOpen",
    "params": {
        "textDocument": {
            "uri": "file:///virtual/project/customization-map.json",
            "languageId": "json",
            "version": 1,
            "text": "{}"
        }
    }
})

for i in range(2):
    line = p.stdout.readline()
    if b"Content-Length" in line:
        p.stdout.readline()
        length = int(line.split(b": ")[1].strip())
        body = p.stdout.read(length)
        print(json.loads(body))
