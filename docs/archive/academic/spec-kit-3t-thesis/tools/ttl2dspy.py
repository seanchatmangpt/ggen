#!/usr/bin/env python3
"""
TTL to DSPy Signature Transpiler
Converts Turtle ontologies with SHACL shapes into DSPy Signature classes

Implements DSPy Core Team recommendations:
- Robust SHACL property discovery (both patterns)
- Safe field name handling with collision detection
- Enhanced CLI with batch mode and return codes
- Performance optimizations and proper error handling
"""

import argparse
import re
import sys
from datetime import datetime
from pathlib import Path
from typing import Dict, List, Set, Tuple

import rdflib
from rdflib import Graph, Namespace
from rdflib.namespace import OWL, RDF, RDFS, SH, XSD

# Define namespaces
CNS = Namespace("http://cns.io/ontology#")
SHACL = Namespace("http://www.w3.org/ns/shacl#")

class TTL2DSPyTranspiler:
    """Converts TTL ontologies to DSPy Signature classes"""

    def __init__(self):
        self.seen_field_names: Set[str] = set()
        self.signature_count = 0

    def safe_local_name(self, iri) -> str:
        """Extract local name safely and efficiently"""
        s = str(iri)
        idx = max(s.rfind('/'), s.rfind('#'))
        return s[idx + 1:] if idx >= 0 else s

    def snake_case(self, name: str) -> str:
        """Convert to snake_case Python identifier"""
        # Replace hyphens and other non-alphanumeric chars with underscores
        name = re.sub(r'[-\s]+', '_', name)
        # Insert underscores before capitals that follow lowercase
        name = re.sub(r'([a-z])([A-Z])', r'\1_\2', name)
        # Convert to lowercase
        name = name.lower()
        # Remove multiple consecutive underscores
        name = re.sub(r'_+', '_', name)
        # Remove leading/trailing underscores
        name = name.strip('_')

        # Ensure it starts with letter or underscore
        if name and name[0].isdigit():
            name = f"field_{name}"

        # Handle empty names
        if not name:
            name = "unnamed_field"

        return name

    def check_field_collision(self, pyname: str, original_iri: str) -> str:
        """Check for field name collisions and resolve them"""
        # Check collision with DSPy Signature reserved names
        reserved_names = {'metadata', 'instructions', 'demos', 'signature', 'config'}

        if hasattr(rdflib.Namespace, pyname) or pyname in reserved_names:
            pyname = f"custom_{pyname}"

        # Handle duplicate field names
        original_pyname = pyname
        counter = 1
        while pyname in self.seen_field_names:
            pyname = f"{original_pyname}_{counter}"
            counter += 1

        self.seen_field_names.add(pyname)
        return pyname

    def extract_datatype(self, prop_shape, g: Graph) -> str:
        """Extract datatype with DSPy type hints"""
        # Check for sh:datatype
        datatype = g.value(prop_shape, SH.datatype)
        if datatype:
            if datatype == XSD.string:
                return "dtype=str"
            elif datatype == XSD.boolean:
                return "dtype=bool"
            elif datatype in (XSD.int, XSD.integer, XSD.long):
                return "dtype=int"
            elif datatype in (XSD.float, XSD.double, XSD.decimal):
                return "dtype=float"

        # Check for sh:class (object properties)
        class_constraint = g.value(prop_shape, SH.class_)
        if class_constraint:
            return "dtype=str"  # Objects referenced as strings

        # Default to string
        return "dtype=str"

    def find_property_shapes(self, cls, g: Graph) -> List:
        """Find property shapes using both direct and node-shape patterns"""
        prop_shapes = set()

        # Pattern 1: Direct sh:targetClass on property shapes
        for prop_shape in g.subjects(SH.path, None):
            if (prop_shape, SH.targetClass, cls) in g:
                prop_shapes.add(prop_shape)

        # Pattern 2: Node shapes with sh:property links
        for node_shape in g.subjects(SH.targetClass, cls):
            for prop_shape in g.objects(node_shape, SH.property):
                if g.value(prop_shape, SH.path):  # Must have a path
                    prop_shapes.add(prop_shape)

        return list(prop_shapes)

    def is_output_field(self, prop_shape, g: Graph) -> bool:
        """Check if property is marked as output field"""
        # Check for cns:outputField annotation
        output_marker = g.value(prop_shape, CNS.outputField)
        if output_marker:
            # Accept various forms of "true"
            output_str = str(output_marker).lower()
            return output_str in ('true', '1', 'yes')

        # Check for rdfs:comment containing "output"
        comment = g.value(prop_shape, RDFS.comment)
        if comment and 'output' in str(comment).lower():
            return True

        return False

    def build_signatures(self, g: Graph) -> Dict[str, str]:
        """Build DSPy Signatures from ontology classes with SHACL shapes"""
        signatures = {}

        # Find all classes with SHACL shapes
        classes_with_shapes = set()

        # Collect classes from both patterns
        for target_class in g.objects(None, SH.targetClass):
            classes_with_shapes.add(target_class)

        for cls in classes_with_shapes:
            cls_name = self.safe_local_name(cls)
            signature_name = f"{cls_name}Signature"

            # Reset field names for each signature
            self.seen_field_names.clear()

            # Find property shapes for this class
            prop_shapes = self.find_property_shapes(cls, g)

            if not prop_shapes:
                continue

            # Build field definitions
            input_fields = []
            output_fields = []

            for prop_shape in prop_shapes:
                path = g.value(prop_shape, SH.path)
                if not path:
                    continue

                prop_name = self.safe_local_name(path)
                py_name = self.snake_case(prop_name)
                py_name = self.check_field_collision(py_name, str(path))

                # Get description
                description = g.value(prop_shape, RDFS.comment) or g.value(prop_shape, SH.description)
                desc_str = f'"{description}"' if description else f'"{prop_name} property"'

                # Get datatype
                dtype = self.extract_datatype(prop_shape, g)

                # Create field definition
                if self.is_output_field(prop_shape, g):
                    field_def = f'    {py_name} = dspy.OutputField(desc={desc_str}, {dtype})'
                    output_fields.append(field_def)
                else:
                    field_def = f'    {py_name} = dspy.InputField(desc={desc_str}, {dtype})'
                    input_fields.append(field_def)

            # Ensure we have exactly one output field (DSPy requirement)
            if len(output_fields) == 0:
                # Add a default output field
                output_fields.append('    result = dspy.OutputField(desc="Generated result", dtype=str)')
            elif len(output_fields) > 1:
                # Keep only the first output field for now
                # TODO: Add --allow-multi-output flag support
                output_fields = output_fields[:1]

            # Get class description
            class_desc = g.value(cls, RDFS.comment) or f"DSPy Signature for {cls_name}"

            # Build signature class
            signature_code = f'''class {signature_name}(dspy.Signature):
    """{class_desc}

    Generated from: {cls}
    Timestamp: {datetime.now().isoformat()}"""

{chr(10).join(input_fields)}
{chr(10).join(output_fields)}
'''

            signatures[signature_name] = signature_code
            self.signature_count += 1

        return signatures

    def generate_module(self, signatures: Dict[str, str], ontology_uri: str = "") -> str:
        """Generate complete Python module with all signatures"""

        # Generate __all__ list
        signature_names = list(signatures.keys())
        all_list = ', '.join(f'"{name}"' for name in signature_names)

        module_code = f'''"""
DSPy Signatures generated from Turtle ontology
Generated by ttl2dspy.py on {datetime.now().isoformat()}

Ontology URI: {ontology_uri}
Signatures generated: {len(signatures)}
"""

import dspy
from typing import Union

# Type aliases for better IDE support
Text = str
Number = Union[int, float]
Boolean = bool

__all__ = [{all_list}]

{chr(10).join(signatures.values())}

# Auto-generated signature registry
SIGNATURES = {{
{chr(10).join(f'    "{name}": {name},' for name in signature_names)}
}}

def get_signature(name: str) -> dspy.Signature:
    """Get signature by name"""
    if name not in SIGNATURES:
        raise ValueError(f"Unknown signature: {{name}}. Available: {{list(SIGNATURES.keys())}}")
    return SIGNATURES[name]

def list_signatures() -> list:
    """List all available signature names"""
    return list(SIGNATURES.keys())
'''

        return module_code

def parse_ontology(ttl_file: Path) -> Tuple[Graph, str]:
    """Parse TTL ontology with error handling"""
    try:
        g = Graph()
        # Performance optimization: disable base URI guessing
        g.parse(ttl_file, format="turtle", publicID="")

        # Try to extract ontology URI
        ontology_uri = ""
        for s, p, o in g.triples((None, RDF.type, OWL.Ontology)):
            ontology_uri = str(s)
            break

        return g, ontology_uri

    except Exception as e:
        print(f"Error parsing {ttl_file}: {e}", file=sys.stderr)
        return None, ""

def write_signature_file(signatures: Dict[str, str], output_file: Path,
                        ontology_uri: str, merge_mode: bool = False) -> bool:
    """Write signatures to Python file with error handling"""
    try:
        transpiler = TTL2DSPyTranspiler()

        if merge_mode or len(signatures) > 1:
            # Generate single module with all signatures
            module_code = transpiler.generate_module(signatures, ontology_uri)
        else:
            # Generate individual signature
            signature_name, signature_code = next(iter(signatures.items()))
            module_code = f'''"""
DSPy Signature: {signature_name}
Generated from ontology: {ontology_uri}
Generated on: {datetime.now().isoformat()}
"""

import dspy

{signature_code}
'''

        output_file.parent.mkdir(parents=True, exist_ok=True)
        output_file.write_text(module_code)
        return True

    except Exception as e:
        print(f"Error writing {output_file}: {e}", file=sys.stderr)
        return False

def main():
    parser = argparse.ArgumentParser(
        description="Convert Turtle ontologies with SHACL shapes to DSPy Signatures",
        formatter_class=argparse.RawDescriptionHelpFormatter,
        epilog="""
Examples:
  python ttl2dspy.py ontology.ttl signature.py
  python ttl2dspy.py ontologies/*.ttl signatures/
  python ttl2dspy.py --merge ontologies/ all_signatures.py
  python ttl2dspy.py --batch ontologies/ output_dir/
        """
    )

    parser.add_argument('input', nargs='+',
                       help='Input TTL files or directories (supports globs)')
    parser.add_argument('output',
                       help='Output Python file or directory')
    parser.add_argument('--merge', action='store_true',
                       help='Combine all signatures into single module')
    parser.add_argument('--batch', action='store_true',
                       help='Process multiple files, one output per input')
    parser.add_argument('--allow-multi-output', action='store_true',
                       help='Allow multiple output fields per signature')
    parser.add_argument('--verbose', '-v', action='store_true',
                       help='Verbose output')

    args = parser.parse_args()

    # Collect input files
    input_files = []
    for input_path in args.input:
        path = Path(input_path)
        if path.is_file() and path.suffix in ('.ttl', '.turtle', '.n3'):
            input_files.append(path)
        elif path.is_dir():
            input_files.extend(path.rglob('*.ttl'))
            input_files.extend(path.rglob('*.turtle'))
        else:
            # Try glob pattern
            import glob
            matched = glob.glob(str(path))
            input_files.extend(Path(f) for f in matched if Path(f).suffix in ('.ttl', '.turtle', '.n3'))

    if not input_files:
        print("No TTL files found", file=sys.stderr)
        return 2

    output_path = Path(args.output)
    success_count = 0
    error_count = 0

    if args.batch:
        # Batch mode - one output file per input
        output_path.mkdir(parents=True, exist_ok=True)

        for ttl_file in input_files:
            if args.verbose:
                print(f"Processing {ttl_file}...")

            g, ontology_uri = parse_ontology(ttl_file)
            if g is None:
                error_count += 1
                continue

            transpiler = TTL2DSPyTranspiler()
            signatures = transpiler.build_signatures(g)

            if not signatures:
                if args.verbose:
                    print(f"No SHACL shapes found in {ttl_file}")
                continue

            output_file = output_path / f"signatures_{ttl_file.stem}.py"
            if write_signature_file(signatures, output_file, ontology_uri):
                success_count += 1
                if args.verbose:
                    print(f"Generated {len(signatures)} signatures -> {output_file}")
            else:
                error_count += 1

    elif args.merge:
        # Merge mode - combine all signatures
        all_signatures = {}
        combined_uri = ""

        for ttl_file in input_files:
            if args.verbose:
                print(f"Processing {ttl_file}...")

            g, ontology_uri = parse_ontology(ttl_file)
            if g is None:
                error_count += 1
                continue

            transpiler = TTL2DSPyTranspiler()
            signatures = transpiler.build_signatures(g)

            if signatures:
                all_signatures.update(signatures)
                if not combined_uri:
                    combined_uri = ontology_uri

        if all_signatures:
            if write_signature_file(all_signatures, output_path, combined_uri, merge_mode=True):
                success_count = 1
                if args.verbose:
                    print(f"Generated {len(all_signatures)} signatures -> {output_path}")
            else:
                error_count = 1

    else:
        # Single file mode
        if len(input_files) > 1:
            print("Multiple input files require --batch or --merge mode", file=sys.stderr)
            return 2

        ttl_file = input_files[0]
        if args.verbose:
            print(f"Processing {ttl_file}...")

        g, ontology_uri = parse_ontology(ttl_file)
        if g is None:
            return 1

        transpiler = TTL2DSPyTranspiler()
        signatures = transpiler.build_signatures(g)

        if not signatures:
            print(f"No SHACL shapes found in {ttl_file}", file=sys.stderr)
            return 1

        if write_signature_file(signatures, output_path, ontology_uri):
            success_count = 1
            if args.verbose:
                print(f"Generated {len(signatures)} signatures -> {output_path}")
        else:
            error_count = 1

    # Print summary
    if args.verbose or error_count > 0:
        print(f"Processed: {success_count} success, {error_count} errors")

    # Return appropriate exit code
    if error_count > 0:
        return 1 if success_count == 0 else 3  # 3 = partial success
    return 0

if __name__ == "__main__":
    sys.exit(main())
