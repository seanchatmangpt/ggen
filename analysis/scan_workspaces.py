#!/usr/bin/env python3
import os
import re
import json
import hashlib
import subprocess

WORKSPACES = {
    "ggen": "/Users/sac/ggen",
    "wasm4pm": "/Users/sac/wasm4pm",
    "wasm4pm-compat": "/Users/sac/wasm4pm-compat",
    "tower-lsp-max": "/Users/sac/tower-lsp-max",
}

EXCLUDE_DIRS = {
    ".git", "target", "node_modules", ".venv", ".venv_shacl", ".agents",
    "~", ".cargo", ".rustup", "target_my_tests", "target_reviewer",
    "target_reviewer2", ".antigravitycli", ".claude", ".claude-flow",
    ".claude-plugin", ".cursor", ".gemini", ".ggen", ".hive-mind",
    ".metrics", ".specify", ".swarm", ".worktrees", "vendors", "wip",
    "target-lsp", "target_lsp", "build", "dist"
}

def get_blake3_digest(filepath):
    try:
        res = subprocess.run(["/opt/homebrew/bin/b3sum", filepath], capture_output=True, text=True, check=True)
        parts = res.stdout.strip().split()
        if parts:
            return parts[0]
    except Exception as e:
        pass
    
    # Fallback to python blake3 package if available
    try:
        import blake3
        h = blake3.blake3()
        with open(filepath, "rb") as f:
            for chunk in iter(lambda: f.read(65536), b""):
                h.update(chunk)
        return h.hexdigest()
    except Exception as e:
        return f"error_computing_blake3: {e}"

def get_sha256_digest(filepath):
    h = hashlib.sha256()
    with open(filepath, "rb") as f:
        for chunk in iter(lambda: f.read(65536), b""):
            h.update(chunk)
    return h.hexdigest()

def scan():
    results = {
        "wasm4pm_bind_receipt": [],
        "execute_command_in_lsp": [],
        "workspace_edit_in_lsp": [],
        "fs_write_in_lsp": [],
        "forbidden_versions": [],
        "shadow_crates": []
    }
    
    # 1. Scan for shadow crates in crates directories of ggen and tower-lsp-max
    forbidden_shadow_names = {"wasm4pm", "wasm4pm-proper", "wasm4pm-compat", "wasm4pm-lsp"}
    for ws_name in ["ggen", "tower-lsp-max"]:
        crates_dir = os.path.join(WORKSPACES[ws_name], "crates")
        if os.path.isdir(crates_dir):
            for entry in os.listdir(crates_dir):
                if entry in forbidden_shadow_names:
                    full_path = os.path.join(crates_dir, entry)
                    results["shadow_crates"].append({
                        "workspace": ws_name,
                        "path": full_path,
                        "name": entry
                    })
    
    # Helper to check if a file path is LSP source file
    def is_lsp_source_file(filepath):
        normalized = filepath.lower()
        if "test" in normalized or "template" in normalized or ".tmpl" in normalized:
            return False
        parts = filepath.split(os.sep)
        has_lsp = any("lsp" in p.lower() for p in parts)
        has_src = "src" in parts
        return has_lsp and has_src and filepath.endswith(".rs")

    # 2. Walk through all workspaces and scan files
    for ws_name, ws_path in WORKSPACES.items():
        if not os.path.isdir(ws_path):
            continue
        for root, dirs, files in os.walk(ws_path, followlinks=False):
            # Prune excluded directories in-place
            dirs[:] = [d for d in dirs if d not in EXCLUDE_DIRS]
            
            for file in files:
                filepath = os.path.join(root, file)
                rel_path = os.path.relpath(filepath, ws_path)
                
                # Check Cargo.toml version
                if file == "Cargo.toml":
                    try:
                        with open(filepath, "r", encoding="utf-8", errors="ignore") as f:
                            content = f.read()
                        matches = re.findall(r'^\s*version\s*=\s*[\'"]1\.0\.0[\'"]', content, re.MULTILINE)
                        if matches:
                            results["forbidden_versions"].append({
                                "workspace": ws_name,
                                "file": filepath,
                                "rel_path": rel_path,
                                "matches": matches
                            })
                    except Exception as e:
                        pass
                
                # Scan code files
                if file.endswith((".rs", ".ts", ".js", ".toml", ".json", ".md", ".tmpl")):
                    try:
                        with open(filepath, "r", encoding="utf-8", errors="ignore") as f:
                            lines = f.readlines()
                        
                        is_lsp_src = is_lsp_source_file(filepath)
                        
                        for idx, line in enumerate(lines, 1):
                            # a. Check for wasm4pm.bind_receipt
                            if "wasm4pm.bind_receipt" in line:
                                is_test = "test" in filepath.lower() or "template" in filepath.lower() or ".tmpl" in filepath or "original_request" in filepath.lower() or "convo.txt" in filepath.lower() or "briefing" in filepath.lower() or "handoff" in filepath.lower() or "audit" in filepath.lower()
                                results["wasm4pm_bind_receipt"].append({
                                    "workspace": ws_name,
                                    "file": filepath,
                                    "rel_path": rel_path,
                                    "line_number": idx,
                                    "content": line.strip(),
                                    "is_violation": is_lsp_src or not is_test
                                })
                            
                            # Checks specific to LSP source files
                            if is_lsp_src:
                                # b. Check for execute_command
                                if "execute_command" in line or "executeCommand" in line:
                                    results["execute_command_in_lsp"].append({
                                        "workspace": ws_name,
                                        "file": filepath,
                                        "rel_path": rel_path,
                                        "line_number": idx,
                                        "content": line.strip()
                                    })
                                
                                # c. Check for WorkspaceEdit
                                if "WorkspaceEdit" in line:
                                    results["workspace_edit_in_lsp"].append({
                                        "workspace": ws_name,
                                        "file": filepath,
                                        "rel_path": rel_path,
                                        "line_number": idx,
                                        "content": line.strip()
                                    })
                                
                                # d. Check for fs::write or std::fs::write in LSP
                                if any(x in line for x in ["std::fs::write", "fs::write", "File::create", "OpenOptions"]):
                                    results["fs_write_in_lsp"].append({
                                        "workspace": ws_name,
                                        "file": filepath,
                                        "rel_path": rel_path,
                                        "line_number": idx,
                                        "content": line.strip()
                                    })
                    except Exception as e:
                        pass

    return results

def write_markdown_report(results, report_path):
    with open(report_path, "w", encoding="utf-8") as f:
        f.write("# Forbidden Patterns Integrity Scan Report\n\n")
        f.write("This report presents the findings of a scan across all active workspaces for forbidden patterns.\n\n")
        
        f.write("## Workspaces Scanned\n")
        for ws, path in WORKSPACES.items():
            f.write(f"- **{ws}**: `{path}`\n")
        f.write("\n")
        
        # Shadow Crates
        f.write("## 1. Shadow Crates\n")
        if results["shadow_crates"]:
            f.write("| Workspace | Name | Path |\n")
            f.write("|---|---|---|\n")
            for item in results["shadow_crates"]:
                f.write(f"| {item['workspace']} | `{item['name']}` | `{item['path']}` |\n")
        else:
            f.write("✓ No forbidden shadow crates found.\n")
        f.write("\n")
        
        # Forbidden versions
        f.write("## 2. Forbidden Cargo Crate Versions (`version = '1.0.0'`)\n")
        if results["forbidden_versions"]:
            f.write("| Workspace | File | Matching Line |\n")
            f.write("|---|---|---|\n")
            for item in results["forbidden_versions"]:
                matches_str = ", ".join([f"`{m.strip()}`" for m in item['matches']])
                f.write(f"| {item['workspace']} | `{item['rel_path']}` | {matches_str} |\n")
        else:
            f.write("✓ No forbidden version constraints found.\n")
        f.write("\n")
        
        # wasm4pm.bind_receipt
        f.write("## 3. wasm4pm.bind_receipt Occurrences\n")
        violations = [item for item in results["wasm4pm_bind_receipt"] if item["is_violation"]]
        references = [item for item in results["wasm4pm_bind_receipt"] if not item["is_violation"]]
        
        f.write("### Violations (LSP or source code context)\n")
        if violations:
            f.write("| Workspace | File | Line | Content |\n")
            f.write("|---|---|---|---|\n")
            for item in violations:
                f.write(f"| {item['workspace']} | `{item['rel_path']}` | {item['line_number']} | `{item['content']}` |\n")
        else:
            f.write("✓ No violations of `wasm4pm.bind_receipt` found in active source code paths.\n")
        f.write("\n")
        
        f.write("### References (Tests, templates, or documentation)\n")
        if references:
            f.write("<details>\n<summary>Click to view references</summary>\n\n")
            f.write("| Workspace | File | Line | Content |\n")
            f.write("|---|---|---|---|\n")
            for item in references:
                content = item['content']
                if len(content) > 100:
                    content = content[:97] + "..."
                f.write(f"| {item['workspace']} | `{item['rel_path']}` | {item['line_number']} | `{content}` |\n")
            f.write("\n</details>\n")
        else:
            f.write("No references found.\n")
        f.write("\n")
        
        # execute_command in LSP
        f.write("## 4. execute_command in LSP Source Files\n")
        if results["execute_command_in_lsp"]:
            f.write("| Workspace | File | Line | Content |\n")
            f.write("|---|---|---|---|\n")
            for item in results["execute_command_in_lsp"]:
                f.write(f"| {item['workspace']} | `{item['rel_path']}` | {item['line_number']} | `{item['content']}` |\n")
        else:
            f.write("✓ No `execute_command` implementations or calls found in LSP source files.\n")
        f.write("\n")
        
        # WorkspaceEdit in LSP
        f.write("## 5. WorkspaceEdit in LSP Source Files (Direct Receipt Binding)\n")
        if results["workspace_edit_in_lsp"]:
            f.write("| Workspace | File | Line | Content |\n")
            f.write("|---|---|---|---|\n")
            for item in results["workspace_edit_in_lsp"]:
                f.write(f"| {item['workspace']} | `{item['rel_path']}` | {item['line_number']} | `{item['content']}` |\n")
        else:
            f.write("✓ No `WorkspaceEdit` uses found in LSP source files.\n")
        f.write("\n")
        
        # fs::write in LSP
        f.write("## 6. std::fs::write in LSP Source Files\n")
        if results["fs_write_in_lsp"]:
            f.write("| Workspace | File | Line | Content |\n")
            f.write("|---|---|---|---|\n")
            for item in results["fs_write_in_lsp"]:
                f.write(f"| {item['workspace']} | `{item['rel_path']}` | {item['line_number']} | `{item['content']}` |\n")
        else:
            f.write("✓ No direct filesystem write methods found in LSP source files.\n")
        f.write("\n")

def main():
    print("Starting integrity scan...")
    results = scan()
    
    os.makedirs("/Users/sac/ggen/analysis", exist_ok=True)
    json_path = "/Users/sac/ggen/analysis/scan_report.json"
    md_path = "/Users/sac/ggen/analysis/scan_report.md"
    
    with open(json_path, "w", encoding="utf-8") as f:
        json.dump(results, f, indent=2)
        
    write_markdown_report(results, md_path)
    
    print(f"Report written to {json_path}")
    print(f"Report written to {md_path}")
    
    json_sha256 = get_sha256_digest(json_path)
    json_blake3 = get_blake3_digest(json_path)
    
    md_sha256 = get_sha256_digest(md_path)
    md_blake3 = get_blake3_digest(md_path)
    
    print("\n=== CRYPTOGRAPHIC DIGESTS ===")
    print(f"JSON Report ({json_path}):")
    print(f"  SHA-256: {json_sha256}")
    print(f"  BLAKE3:  {json_blake3}")
    print(f"Markdown Report ({md_path}):")
    print(f"  SHA-256: {md_sha256}")
    print(f"  BLAKE3:  {md_blake3}")

if __name__ == "__main__":
    main()
