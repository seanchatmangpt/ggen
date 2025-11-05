# RDF Parser Libraries for C

## Raptor RDF Syntax Library (Recommended)

**Official Website**: https://librdf.org/raptor/

**Features**:
- Free, open-source C library
- Supports multiple RDF formats:
  - RDF/XML
  - N-Triples
  - Turtle (TTL)
  - N3
  - JSON-LD (via Redland)
- Stream-based parsing (memory efficient)
- Can work standalone or with Redland RDF library
- Portable across POSIX systems

**Installation**:

### macOS (Homebrew):
```bash
brew install raptor
```

### Linux (Debian/Ubuntu):
```bash
sudo apt-get install libraptor2-dev
```

### From Source:
```bash
wget http://download.librdf.org/source/raptor2-2.0.16.tar.gz
tar -xzf raptor2-2.0.16.tar.gz
cd raptor2-2.0.16
./configure && make && sudo make install
```

**Basic Usage Example**:
```c
#include <raptor2/raptor2.h>

// Parse Turtle file
raptor_world *world = raptor_new_world();
raptor_parser *parser = raptor_new_parser(world, "turtle");

// Set statement handler callback
raptor_parser_set_statement_handler(parser, NULL, statement_handler);

// Parse file
FILE *file = fopen("data.ttl", "r");
raptor_parser_parse_file_stream(parser, file, NULL, "data.ttl");

raptor_free_parser(parser);
raptor_free_world(world);
```

**Compilation**:
```bash
clang -O3 -std=c11 your_file.c -o your_program \
  $(pkg-config --cflags --libs raptor2)
```

## Redland RDF Application Framework

**Components**:
1. **Raptor** - RDF parser/serializer
2. **Rasqal** - SPARQL query library
3. **Redland** - RDF storage (triple stores)

**Installation**:
```bash
# macOS
brew install redland

# Linux
sudo apt-get install librdf0-dev
```

## Alternative: Lightweight Options

### Simple N-Triples Parser
For simple use cases, you could write a minimal parser for N-Triples format (one triple per line, simple syntax).

### Other C Libraries
- **Serd** - Lightweight RDF syntax library (C, minimal dependencies)
  - GitHub: https://github.com/drobilla/serd
  - Good for embedded systems

## Integration with POC

For the `knhk_8tick_poc.c`, you could:
1. Use Raptor to parse RDF/Turtle files into triples
2. Extract S, P, O arrays (Subject, Predicate, Object)
3. Feed into your SIMD-optimized query engine

**Example Integration**:
```c
#include <raptor2/raptor2.h>

typedef struct {
    uint64_t *S, *P, *O;
    size_t count;
} triple_array_t;

void statement_handler(void *user_data, raptor_statement *statement) {
    triple_array_t *arr = (triple_array_t*)user_data;
    // Extract S, P, O from statement
    // Convert to uint64_t IDs and add to arrays
}
```

