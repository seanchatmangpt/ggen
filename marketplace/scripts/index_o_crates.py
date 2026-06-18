#!/usr/bin/env python3
import os
import json
import subprocess
from pathlib import Path
from datetime import datetime, timezone

def compute_blake3_directory(directory_path):
    """Computes a BLAKE3 hash of all files in a directory by sorting them and hashing their contents."""
    files = []
    for root, _, filenames in os.walk(directory_path):
        for f in filenames:
            if f.endswith('.ttl') or f.endswith('.rq') or f.endswith('.shacl'):
                files.append(os.path.join(root, f))
    
    files.sort()
    
    if not files:
        return "empty"
        
    hasher = subprocess.Popen(["b3sum", "--no-names"], stdin=subprocess.PIPE, stdout=subprocess.PIPE)
    for batch_start in range(0, len(files), 100):
        batch = files[batch_start:batch_start+100]
        cmd = ["b3sum"] + batch
        result = subprocess.run(cmd, capture_output=True, text=True)
        hasher.stdin.write(result.stdout.encode('utf-8'))
    
    out, _ = hasher.communicate()
    return out.decode('utf-8').strip().split()[0]

def main():
    base_dir = Path(__file__).parent.parent.parent
    ontology_catalogue = base_dir / "ontology_catalogue"
    registry_file = base_dir / "marketplace" / "registry" / "index.json"
    
    if not ontology_catalogue.exists():
        print(f"Error: {ontology_catalogue} does not exist.")
        return
        
    crates = []
    
    for item in os.listdir(ontology_catalogue):
        crate_path = ontology_catalogue / item
        if crate_path.is_dir():
            ttl_count = 0
            rq_count = 0
            for root, _, files in os.walk(crate_path):
                for f in files:
                    if f.endswith('.ttl') or f.endswith('.shacl'):
                        ttl_count += 1
                    elif f.endswith('.rq'):
                        rq_count += 1
            
            if ttl_count > 0 or rq_count > 0:
                crate_hash = compute_blake3_directory(crate_path)
                crates.append({
                    "id": item,
                    "version": "1.0.0",
                    "ontologyCount": ttl_count,
                    "queryCount": rq_count,
                    "blake3Hash": crate_hash,
                    "path": f"ontology_catalogue/{item}"
                })
                print(f"Indexed O-Crate: {item} ({ttl_count} ontologies, {rq_count} queries)")
                
    if registry_file.exists():
        with open(registry_file, 'r') as f:
            registry = json.load(f)
    else:
        registry = {}
        
    registry['o_crates'] = crates
    registry['o_crate_count'] = len(crates)
    registry['updated_at'] = datetime.now(timezone.utc).isoformat().replace('+00:00', 'Z')
    
    with open(registry_file, 'w') as f:
        json.dump(registry, f, indent=2)
        
    print(f"\nSuccessfully indexed {len(crates)} O-Crates into marketplace registry.")

if __name__ == "__main__":
    main()
