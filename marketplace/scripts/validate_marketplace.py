#!/usr/bin/env python3
import os
import sys
import json
from pathlib import Path

try:
    import tomllib  # Python 3.11+
except ImportError:
    try:
        import tomli as tomllib  # Python < 3.11
    except ImportError:
        print("Error: tomli library required. Install with: pip install tomli")
        sys.exit(1)

# Helper to write toml (since tomllib is read-only)
def write_toml(data, file_path):
    # Standard manual serialization to preserve comments and spacing as much as possible
    # or simple representation if needed
    lines = []
    
    # Let's organize into tables
    # First top-level or [package]
    if "package" in data:
        lines.append("[package]")
        for k, v in data["package"].items():
            if isinstance(v, str):
                lines.append(f'{k} = "{v}"')
            elif isinstance(v, list):
                val_str = ", ".join(f'"{x}"' for x in v)
                lines.append(f'{k} = [{val_str}]')
            elif isinstance(v, bool):
                lines.append(f'{k} = {"true" if v else "false"}')
            elif isinstance(v, int):
                lines.append(f'{k} = {v}')
        lines.append("")

    for table_name, table_data in data.items():
        if table_name == "package":
            continue
        lines.append(f"[{table_name}]")
        for k, v in table_data.items():
            if isinstance(v, str):
                lines.append(f'{k} = "{v}"')
            elif isinstance(v, list):
                val_str = ", ".join(f'"{x}"' for x in v)
                lines.append(f'{k} = [{val_str}]')
            elif isinstance(v, bool):
                lines.append(f'{k} = {"true" if v else "false"}')
            elif isinstance(v, int):
                lines.append(f'{k} = {v}')
        lines.append("")
        
    with open(file_path, 'w') as f:
        f.write("\n".join(lines))

def sanitize_toml_file(file_path):
    with open(file_path, 'r', encoding='utf-8') as f:
        content = f.read()
    
    lines = content.splitlines()
    cleaned_lines = []
    
    current_section = ("table", "")
    # Keep track of array indices
    array_counters = {}
    # Map from section_id -> list of line indices where each key is defined
    key_definitions = {}
    
    import re
    # We want to identify the headers
    array_table_pat = re.compile(r'^\s*\[\[([^\]]+)\]\]')
    standard_table_pat = re.compile(r'^\s*\[([^\[\]]+)\]')
    key_pat = re.compile(r'^\s*([a-zA-Z0-9_-]+)\s*=')
    
    for idx, line in enumerate(lines):
        line_strip = line.split('#')[0].strip()
        if not line_strip:
            continue
            
        array_match = array_table_pat.match(line_strip)
        if array_match:
            name = array_match.group(1).strip()
            array_counters[name] = array_counters.get(name, -1) + 1
            current_section = ("array", name, array_counters[name])
            continue
            
        sec_match = standard_table_pat.match(line_strip)
        if sec_match:
            name = sec_match.group(1).strip()
            current_section = ("table", name)
            continue
            
        key_match = key_pat.match(line_strip)
        if key_match:
            key = key_match.group(1)
            key_def = (current_section, key)
            if key_def not in key_definitions:
                key_definitions[key_def] = []
            key_definitions[key_def].append(idx)
            
    skip_indices = set()
    cleaned_duplicates = []
    for (sec_info, key), indices in key_definitions.items():
        if len(indices) > 1:
            for idx in indices[:-1]:
                skip_indices.add(idx)
            # Format section name for logging
            if sec_info[0] == "table":
                sec_str = f"[{sec_info[1]}]" if sec_info[1] else "root"
            else:
                sec_str = f"[[{sec_info[1]}]] (index {sec_info[2]})"
            cleaned_duplicates.append(f"Removed duplicate key '{key}' in section '{sec_str}'")
            
    if not skip_indices:
        return False, cleaned_duplicates
        
    new_lines = [lines[idx] for idx in range(len(lines)) if idx not in skip_indices]
    
    with open(file_path, 'w', encoding='utf-8') as f:
        f.write('\n'.join(new_lines) + '\n')
        
    return True, cleaned_duplicates


def main():
    import argparse
    parser = argparse.ArgumentParser()
    parser.add_argument("--packages-dir", required=True)
    parser.add_argument("--update", action="store_true")
    parser.add_argument("--json", action="store_true")
    parser.add_argument("package", nargs="?", default=None)
    args = parser.parse_args()
    
    packages_dir = Path(args.packages_dir)
    if not packages_dir.exists():
        print(f"Error: Packages directory not found: {packages_dir}", file=sys.stderr)
        sys.exit(1)
        
    total_packages = 0
    ready_count = 0
    needs_improvement_count = 0
    not_ready_count = 0
    all_results = []
    
    for package_dir in sorted(packages_dir.iterdir()):
        if not package_dir.is_dir():
            continue
        
        package_name = package_dir.name
        # Skip hidden folders
        if package_name.startswith('.'):
            continue
            
        # If a specific package is requested, skip all others
        if args.package and args.package != package_name:
            continue
            
        package_toml_path = package_dir / "package.toml"
        if not package_toml_path.exists():
            continue
            
        total_packages += 1
        
        # Check and sanitize duplicate keys if --update is specified
        cleaned_up = []
        if args.update:
            try:
                was_sanitized, cleanup_msgs = sanitize_toml_file(package_toml_path)
                if was_sanitized:
                    cleaned_up.extend(cleanup_msgs)
            except Exception as e:
                pass
                
        # Parse package.toml
        try:
            with open(package_toml_path, "rb") as f:
                toml_data = tomllib.load(f)
        except Exception as e:
            # Failed to parse
            all_results.append({
                "package_name": package_name,
                "score": 0.0,
                "production_ready": False,
                "errors": [f"Failed to parse package.toml: {e}"],
                "warnings": [],
                "required_checks": [],
                "quality_checks": []
            })
            not_ready_count += 1
            continue
            
        pkg = toml_data.get("package", {})
        
        # 1. Metadata Check
        has_name = bool(pkg.get("name"))
        has_desc = bool(pkg.get("description"))
        
        authors = pkg.get("authors") or []
        if isinstance(authors, str):
            authors = [authors]
        elif not authors and pkg.get("author"):
            authors = [pkg.get("author")]
            
        has_authors = len(authors) > 0
        metadata_passed = has_name and has_desc and has_authors
        
        # 2. License Check
        license_str = pkg.get("license", "")
        valid_licenses = ["MIT", "Apache-2.0", "GPL-3.0", "BSD-3-Clause", "ISC"]
        license_passed = any(l in license_str for l in valid_licenses) or "Custom" in license_str
        
        # 3. README Check
        has_readme = False
        for entry in package_dir.iterdir():
            if entry.is_file() and entry.name.upper().startswith("README"):
                has_readme = True
                break
                
        # 4. Repository Check
        has_repo = bool(pkg.get("repository"))
        
        # 5. Author Check
        author_passed = has_authors
        
        # Calculate score
        score = 0
        if metadata_passed: score += 20
        if license_passed: score += 25
        if has_readme: score += 20
        if has_repo: score += 15
        if author_passed: score += 20
        
        production_ready = (score >= 95)
        
        if production_ready:
            ready_count += 1
        elif score >= 80:
            needs_improvement_count += 1
        else:
            not_ready_count += 1
            
        required_checks = [
            ["metadata", {"message": "Metadata is complete" if metadata_passed else "Missing package name or description"}],
            ["license", {"message": f"License: {license_str}" if license_passed else "Invalid or missing license specification"}],
            ["readme", {"message": "README file present" if has_readme else "Missing README file in package directory"}],
            ["repository", {"message": "Repository URL provided" if has_repo else "No repository URL"}],
            ["authors", {"message": f"Authors: {', '.join(authors)}" if author_passed else "No authors specified"}]
        ]
        
        # Quality/Bonus checks (represented as optional checks in JSON output)
        quality_checks = []
        
        # If --update flag is specified, we write the production_ready flag back to package.toml
        if args.update:
            with open(package_toml_path, "r") as f:
                content = f.read()
            
            lines = content.splitlines()
            marketplace_start_idx = -1
            marketplace_end_idx = -1
            
            # Find [marketplace] line
            for i, line in enumerate(lines):
                if line.strip() == "[marketplace]":
                    marketplace_start_idx = i
                    break
                    
            if marketplace_start_idx == -1:
                # [marketplace] section doesn't exist, append it at the end
                new_val = "true" if production_ready else "false"
                content = content.rstrip() + f"\n\n[marketplace]\nproduction_ready = {new_val}\n"
            else:
                # Find where the [marketplace] section ends (next section header starting with [name])
                import re
                section_header_pat = re.compile(r"^\s*\[[a-zA-Z0-9_-]+\]")
                for i in range(marketplace_start_idx + 1, len(lines)):
                    if section_header_pat.match(lines[i]):
                        marketplace_end_idx = i
                        break
                if marketplace_end_idx == -1:
                    marketplace_end_idx = len(lines)
                    
                # Extract marketplace lines
                marketplace_lines = lines[marketplace_start_idx : marketplace_end_idx]
                
                # Filter out any existing production_ready definitions
                new_marketplace_lines = [marketplace_lines[0]] # Keep [marketplace]
                new_marketplace_lines.append(f"production_ready = {'true' if production_ready else 'false'}")
                
                # Add other lines, filtering out production_ready
                prod_ready_pat = re.compile(r"^\s*production_ready\s*=")
                for line in marketplace_lines[1:]:
                    if not prod_ready_pat.match(line):
                        new_marketplace_lines.append(line)
                        
                # Reassemble the lines
                result_lines = lines[:marketplace_start_idx] + new_marketplace_lines + lines[marketplace_end_idx:]
                content = "\n".join(result_lines) + "\n"
                
            with open(package_toml_path, "w") as f:
                f.write(content)
                
        all_results.append({
            "package_name": package_name,
            "score": float(score),
            "production_ready": production_ready,
            "errors": [],
            "warnings": cleaned_up,
            "required_checks": required_checks,
            "quality_checks": quality_checks
        })
        
    output = {
        "total_packages": total_packages,
        "ready_count": ready_count,
        "needs_improvement_count": needs_improvement_count,
        "not_ready_count": not_ready_count,
        "all_results": all_results
    }
    
    if args.json:
        print(json.dumps(output, indent=2))
    else:
        # Human readable format
        print(f"Total packages: {total_packages}")
        print(f"Production ready: {ready_count}")
        print(f"Needs improvement: {needs_improvement_count}")
        print(f"Not ready: {not_ready_count}")

if __name__ == "__main__":
    main()
