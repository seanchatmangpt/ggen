#!/bin/bash
# Deep search across ~/ for implementation gaps

echo "Searching for RelationPage and Construct8..."
find ~ -type d \( -name "Library" -o -name "node_modules" -o -name ".rustup" -o -name ".cargo" -o -name ".docker" -o -name "miniconda3" -o -name "opt" -o -name "var" \) -prune -o -type f -name "*.rs" -exec grep -lE "struct RelationPage|struct Construct8|impl Construct8" {} + > found_core.txt

echo "Searching for AtomVM bindings..."
find ~ -type d \( -name "Library" -o -name "node_modules" -o -name ".rustup" -o -name ".cargo" -o -name ".docker" -o -name "miniconda3" -o -name "opt" -o -name "var" \) -prune -o -type f -name "*.rs" -exec grep -lE "AtomVM|nif" {} + > found_atomvm.txt

echo "Searching for Truex / BLAKE3 receipt chain generation..."
find ~ -type d \( -name "Library" -o -name "node_modules" -o -name ".rustup" -o -name ".cargo" -o -name ".docker" -o -name "miniconda3" -o -name "opt" -o -name "var" \) -prune -o -type f -name "*.rs" -exec grep -lE "blake3.*receipt|truex" {} + > found_receipts.txt

echo "Searching for OCEL 2.0 JSON projection generator..."
find ~ -type d \( -name "Library" -o -name "node_modules" -o -name ".rustup" -o -name ".cargo" -o -name ".docker" -o -name "miniconda3" -o -name "opt" -o -name "var" \) -prune -o -type f -name "*.rs" -exec grep -lE "struct Ocel|impl Ocel|ocel.*projection" {} + > found_ocel.txt

echo "Searching for SHACL validation hook..."
find ~ -type d \( -name "Library" -o -name "node_modules" -o -name ".rustup" -o -name ".cargo" -o -name ".docker" -o -name "miniconda3" -o -name "opt" -o -name "var" \) -prune -o -type f -name "*.rs" -exec grep -lE "shacl.*validation|sh:ValidationReport" {} + > found_shacl.txt

echo "Search complete. Results:"
echo "--- Core (RelationPage/Construct8) ---"
cat found_core.txt
echo "--- AtomVM ---"
cat found_atomvm.txt
echo "--- Receipts (Truex/BLAKE3) ---"
cat found_receipts.txt
echo "--- OCEL ---"
cat found_ocel.txt
echo "--- SHACL ---"
cat found_shacl.txt
