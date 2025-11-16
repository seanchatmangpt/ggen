#!/usr/bin/env python3
"""
Generate marketplace registry index.json from all package.toml files.

Scans marketplace/packages/*/package.toml and generates a complete registry index.
"""

import json
import os
import sys
from pathlib import Path
from datetime import datetime, timezone
from collections import defaultdict

try:
    import tomllib  # Python 3.11+
except ImportError:
    try:
        import tomli as tomllib  # Python < 3.11
    except ImportError:
        print("Error: tomli library required. Install with: pip install tomli")
        sys.exit(1)


def calculate_sha256_checksum(package_dir: Path) -> str:
    """Calculate SHA256 checksum of package directory."""
    import hashlib
    import os
    
    sha256 = hashlib.sha256()
    
    # Walk through all files in package directory
    for root, dirs, files in os.walk(package_dir):
        # Skip hidden directories and files
        dirs[:] = [d for d in dirs if not d.startswith('.')]
        
        for file in sorted(files):
            if file.startswith('.'):
                continue
                
            file_path = os.path.join(root, file)
            rel_path = os.path.relpath(file_path, package_dir)
            
            # Hash the relative path
            sha256.update(rel_path.encode('utf-8'))
            
            # Hash the file content
            try:
                with open(file_path, 'rb') as f:
                    sha256.update(f.read())
            except Exception:
                # Skip files that can't be read
                pass
    
    return sha256.hexdigest()


def parse_package_toml(package_dir: Path) -> dict:
    """Parse a package.toml file and extract metadata."""
    package_toml = package_dir / "package.toml"
    
    if not package_toml.exists():
        return None
    
    try:
        with open(package_toml, "rb") as f:
            data = tomllib.load(f)
    except Exception as e:
        print(f"Warning: Failed to parse {package_toml}: {e}", file=sys.stderr)
        return None
    
    package = data.get("package", {})
    package_tags = data.get("package.tags", {})
    package_keywords = data.get("package.keywords", {})
    package_metadata = data.get("package.metadata", {})
    
    # Extract tags - handle both array format and dict format
    tags = []
    if "tags" in package and isinstance(package["tags"], list):
        tags = package["tags"]
    elif package_tags and isinstance(package_tags, dict) and "tags" in package_tags:
        tags = package_tags["tags"] if isinstance(package_tags["tags"], list) else []
    
    # Extract keywords
    keywords = []
    if "keywords" in package and isinstance(package["keywords"], list):
        keywords = package["keywords"]
    elif package_keywords and isinstance(package_keywords, dict) and "keywords" in package_keywords:
        keywords = package_keywords["keywords"] if isinstance(package_keywords["keywords"], list) else []
    
    # Get package name - use id if available, otherwise use directory name
    package_name = package.get("id") or package.get("name") or package_dir.name
    
    # Generate download URL (GitHub archive format)
    # For local development, we'll use a placeholder that can be replaced
    download_url = f"https://github.com/seanchatmangpt/ggen/archive/refs/heads/master.zip"
    
    # Calculate checksum for the package directory
    try:
        checksum = calculate_sha256_checksum(package_dir)
    except Exception as e:
        print(f"Warning: Failed to calculate checksum for {package_dir}: {e}", file=sys.stderr)
        checksum = ""  # Will be calculated at runtime if missing
    
    # Build package info with required fields
    package_info = {
        "name": package_name,
        "version": package.get("version", "1.0.0"),
        "category": package.get("category", "uncategorized"),
        "description": package.get("description", ""),
        "tags": tags,
        "keywords": keywords,
        "author": package.get("author"),
        "license": package.get("license"),
        "downloads": 0,  # Default values
        "stars": 0,
        "production_ready": package_metadata.get("production_ready", False) if isinstance(package_metadata, dict) else False,
        "dependencies": package.get("dependencies", []) if isinstance(package.get("dependencies"), list) else [],
        "path": f"marketplace/packages/{package_name}",
        "download_url": download_url,
        "checksum": checksum,
    }
    
    return package_info


def build_search_index(packages: list) -> dict:
    """Build search index from packages."""
    search_index = defaultdict(list)
    
    for pkg in packages:
        name = pkg["name"]
        
        # Index by tags
        for tag in pkg.get("tags", []):
            search_index[tag.lower()].append(name)
        
        # Index by keywords
        for keyword in pkg.get("keywords", []):
            search_index[keyword.lower()].append(name)
        
        # Index by category
        category = pkg.get("category", "").lower()
        if category:
            search_index[category].append(name)
        
        # Index by name words
        for word in name.split("-"):
            if len(word) > 2:  # Skip short words
                search_index[word.lower()].append(name)
    
    # Deduplicate
    return {k: sorted(list(set(v))) for k, v in search_index.items()}


def main():
    """Generate registry index from all packages."""
    # Get script directory and project root
    script_dir = Path(__file__).parent
    project_root = script_dir.parent.parent
    packages_dir = project_root / "marketplace" / "packages"
    registry_dir = project_root / "marketplace" / "registry"
    
    if not packages_dir.exists():
        print(f"Error: Packages directory not found: {packages_dir}", file=sys.stderr)
        sys.exit(1)
    
    # Scan all packages
    packages = []
    categories = defaultdict(int)
    
    for package_dir in sorted(packages_dir.iterdir()):
        if not package_dir.is_dir():
            continue
        
        package_info = parse_package_toml(package_dir)
        if package_info:
            packages.append(package_info)
            category = package_info["category"]
            categories[category] += 1
    
    # Build search index
    search_index = build_search_index(packages)
    
    # Generate registry index
    registry_index = {
        "version": "1.0.0",
        "registry_url": "https://github.com/seanchatmangpt/ggen",
        "updated_at": datetime.now(timezone.utc).isoformat().replace("+00:00", "Z"),
        "package_count": len(packages),
        "categories": dict(categories),
        "packages": packages,
        "search_index": search_index,
    }
    
    # Write to file
    registry_dir.mkdir(parents=True, exist_ok=True)
    output_file = registry_dir / "index.json"
    
    with open(output_file, "w") as f:
        json.dump(registry_index, f, indent=2)
    
    print(f"✅ Generated registry index: {output_file}")
    print(f"   - {len(packages)} packages indexed")
    print(f"   - {len(categories)} categories")
    print(f"   - {len(search_index)} search index entries")
    
    return 0


if __name__ == "__main__":
    sys.exit(main())

