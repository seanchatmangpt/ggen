import json
import os

# Traverse brain directory using os.walk to include hidden folders like .system_generated
log_files = []
for root, dirs, files in os.walk("/Users/sac/.gemini/antigravity-cli/brain/"):
    for file in files:
        if file.startswith("transcript") and file.endswith(".jsonl"):
            log_files.append(os.path.join(root, file))

recovered_files = {}

for log_path in log_files:
    if not os.path.exists(log_path):
        continue
    print(f"Scanning log: {log_path}")
    with open(log_path, "r", errors="ignore") as f:
        for line in f:
            try:
                data = json.loads(line)
            except Exception:
                continue
            
            def find_write(obj):
                if isinstance(obj, dict):
                    if "TargetFile" in obj and "CodeContent" in obj:
                        target = obj["TargetFile"]
                        content = obj["CodeContent"]
                        
                        # Strip literal quotes if present
                        if isinstance(target, str):
                            target = target.strip('"\'')
                        if isinstance(content, str):
                            content = content.strip('"\'')
                            
                        # Munge escaped newlines and tab characters if they were double-serialized
                        if isinstance(content, str) and "\\n" in content:
                            try:
                                content = json.loads(f'"{content}"')
                            except Exception:
                                content = content.replace("\\n", "\n").replace("\\t", "\t").replace('\\"', '"')
                        
                        if "crates/ggen-graph/" in target:
                            recovered_files[target] = content
                    for k, v in obj.items():
                        find_write(v)
                elif isinstance(obj, list):
                    for item in obj:
                        find_write(item)
            
            find_write(data)

print(f"Found {len(recovered_files)} files in logs.")
for target, content in recovered_files.items():
    if not content:
        continue
    target = target.strip('"\'')
    os.makedirs(os.path.dirname(target), exist_ok=True)
    with open(target, "w") as out:
        out.write(content)
    print(f"Restored: {target} ({len(content)} bytes)")
