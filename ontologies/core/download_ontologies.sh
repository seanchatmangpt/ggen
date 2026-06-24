#!/bin/bash
set -e

# Create a manifest of W3C canonical ontology sources
declare -A ONTOLOGIES=(
    ["rdf-syntax-ns.ttl"]="https://www.w3.org/1999/02/22-rdf-syntax-ns#"
    ["rdf-schema.ttl"]="https://www.w3.org/2000/01/rdf-schema#"
    ["owl.ttl"]="https://www.w3.org/2002/07/owl#"
    ["skos.ttl"]="https://www.w3.org/2004/02/skos/core#"
    ["skos-xl.ttl"]="https://www.w3.org/2008/05/skos-xl#"
    ["dcterms.ttl"]="https://purl.org/dc/terms/"
    ["dcat.ttl"]="https://www.w3.org/ns/dcat#"
)

echo "Downloading W3C canonical ontologies..."
echo ""

for filename in "${!ONTOLOGIES[@]}"; do
    url="${ONTOLOGIES[$filename]}"
    
    # Try to get the canonical RDF/TTL version
    # W3C typically serves .ttl or uses content negotiation
    echo "Downloading: $filename"
    echo "  URL: $url"
    
    # Use curl with Accept header for Turtle format
    if curl -s -L \
        -H "Accept: text/turtle, application/rdf+xml" \
        -o "$filename" \
        "$url" 2>/dev/null; then
        
        # Check if file was actually downloaded and has content
        if [ -s "$filename" ]; then
            size=$(du -h "$filename" | cut -f1)
            echo "  Status: ✓ Downloaded ($size)"
        else
            echo "  Status: ✗ Empty response, retrying with alternative URL"
            rm -f "$filename"
        fi
    else
        echo "  Status: ✗ Failed to download"
    fi
    echo ""
done

echo "Download complete. Final manifest:"
ls -lh *.ttl *.rdf 2>/dev/null || true
