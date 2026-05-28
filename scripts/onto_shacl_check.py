#!/usr/bin/env python3
import sys
from pyshacl import validate
from rdflib import Graph

def main():
    if len(sys.argv) < 3:
        print("Usage: onto_shacl_check.py <data.ttl> <shacl.ttl>")
        sys.exit(1)

    data_file = sys.argv[1]
    shacl_file = sys.argv[2]

    g = Graph()
    g.parse(data_file, format="turtle")
    
    sg = Graph()
    sg.parse(shacl_file, format="turtle")

    conforms, results_graph, results_text = validate(
        g,
        shacl_graph=sg,
        inference='rdfs',
        debug=False,
    )

    if conforms:
        print("SHACL validation passed.")
        sys.exit(0)
    else:
        print("SHACL validation failed:")
        print(results_text)
        sys.exit(1)

if __name__ == "__main__":
    main()
