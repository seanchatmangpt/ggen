# Docker Deployment Guide for ggen

## Quick Start (For Users)

```bash
# Pull the image from Docker Hub
docker pull seanchatman/ggen:5.0.2

# Run ggen in a container
docker run --rm -v $(pwd):/workspace seanchatman/ggen:5.0.2 sync

# Interactive shell with ggen
docker run --rm -it -v $(pwd):/workspace seanchatman/ggen:5.0.2 bash
```

## Example: Following the README Tutorial

**Step 1: Create project structure**
```bash
mkdir ggen-docker-demo && cd ggen-docker-demo
mkdir schema
```

**Step 2: Create ontology** (`schema/domain.ttl`)
```turtle
@prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .
@prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .
@prefix ex: <https://example.com/> .

ex:Person a rdfs:Class ;
    rdfs:label "Person" ;
    rdfs:comment "Represents a person in the system" .

ex:name a rdf:Property ;
    rdfs:domain ex:Person ;
    rdfs:range rdfs:Literal ;
    rdfs:label "name" .
```

**Step 3: Create config** (`ggen.toml`)
```toml
[project]
name = "docker-demo"
version = "0.1.0"

[ontology]
source = "schema/domain.ttl"
base_iri = "https://example.com/"

[[generation.rules]]
name = "structs"
query = { inline = """
SELECT ?name WHERE { ?class a rdfs:Class ; rdfs:label ?name }
""" }
template = { inline = """
{% for row in results %}
pub struct {{ row.name }} { pub name: String }
{% endfor %}
""" }
output_file = "generated/structs.rs"

[generation]
output_dir = "generated"
```

**Step 4: Run ggen sync**
```bash
docker run --rm -v $(pwd):/workspace seanchatman/ggen:5.0.0 sync

# Or with docker compose
docker compose run --rm ggen sync
```

## Docker Compose Setup

Create `docker-compose.yml`:

```yaml
version: '3.8'

services:
  ggen:
    image: seanchatman/ggen:5.0.0
    volumes:
      - .:/workspace
    working_dir: /workspace
    command: sync
```

Usage:
```bash
docker compose run --rm ggen sync
docker compose run --rm ggen --help
```

## Building the Image (For Contributors)

### Option 1: Multi-Stage Build (recommended)
```bash
# Build from source (takes 5-10 minutes)
docker build -t seanchatman/ggen:5.0.0 .

# Tag as latest
docker tag seanchatman/ggen:5.0.0 seanchatman/ggen:latest
```

### Option 2: Binary-Based Build (faster)
```bash
# First, build the binary locally
cargo build --release --bin ggen --manifest-path crates/ggen-cli/Cargo.toml

# Build Docker image with prebuilt binary (takes ~30 seconds)
docker build -f Dockerfile.binary -t seanchatman/ggen:5.0.0 .
```

### Push to Docker Hub
```bash
# Login to Docker Hub
docker login

# Push versioned image
docker push seanchatman/ggen:5.0.0

# Push latest tag
docker tag seanchatman/ggen:5.0.0 seanchatman/ggen:latest
docker push seanchatman/ggen:latest
```

## Image Details

- **Base Image**: `debian:bookworm-slim`
- **Size**: ~50MB (compressed), ~150MB (uncompressed)
- **Architecture**: linux/amd64, linux/arm64 (multi-platform)
- **Working Directory**: `/workspace`
- **Binary Location**: `/usr/local/bin/ggen`

## Multi-Platform Builds

To build for multiple architectures:

```bash
# Enable buildx
docker buildx create --use

# Build and push multi-platform
docker buildx build \
  --platform linux/amd64,linux/arm64 \
  -t seanchatman/ggen:5.0.0 \
  -t seanchatman/ggen:latest \
  --push \
  .
```

## CI/CD Integration

### GitHub Actions Example

```yaml
name: Docker Build and Push

on:
  release:
    types: [published]

jobs:
  docker:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v4

      - name: Set up Docker Buildx
        uses: docker/setup-buildx-action@v3

      - name: Login to Docker Hub
        uses: docker/login-action@v3
        with:
          username: ${{ secrets.DOCKERHUB_USERNAME }}
          password: ${{ secrets.DOCKERHUB_TOKEN }}

      - name: Build and push
        uses: docker/build-push-action@v5
        with:
          context: .
          platforms: linux/amd64,linux/arm64
          push: true
          tags: |
            seanchatman/ggen:${{ github.event.release.tag_name }}
            seanchatman/ggen:latest
```

## Troubleshooting

### Permission Issues

If you encounter permission issues with generated files:

```bash
# Run with current user
docker run --rm --user $(id -u):$(id -g) \
  -v $(pwd):/workspace \
  seanchatman/ggen:5.0.0 sync
```

### Volume Mounting on Windows

```powershell
# PowerShell
docker run --rm -v ${PWD}:/workspace seanchatman/ggen:5.0.0 sync

# CMD
docker run --rm -v %cd%:/workspace seanchatman/ggen:5.0.0 sync
```

### Debugging

```bash
# Interactive shell
docker run --rm -it -v $(pwd):/workspace seanchatman/ggen:5.0.0 bash

# Check binary
docker run --rm seanchatman/ggen:5.0.0 ggen --version

# View help
docker run --rm seanchatman/ggen:5.0.0 ggen --help
```

## Performance

- **Binary install**: ~1 second (using Dockerfile.binary)
- **Multi-stage build**: ~5-10 minutes (compiles from source)
- **Image pull**: ~30 seconds (50MB download)
- **Container startup**: <1 second
- **ggen sync**: Same performance as native binary

## Security

- Runs as root by default (can override with `--user`)
- No SSH keys or credentials included
- Minimal base image (Debian slim)
- Only runtime dependencies (ca-certificates, libssl3)
- No build tools in final image

## Alternatives

If Docker seems heavy, consider:

1. **Homebrew** (macOS/Linux): `brew install seanchatmangpt/ggen/ggen`
2. **Cargo**: `cargo install ggen-cli-lib`
3. **Binary Download**: Download from GitHub Releases
