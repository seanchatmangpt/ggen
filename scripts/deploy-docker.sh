#!/bin/bash
# Docker Hub Deployment Script for ggen v5.0.0
# This script builds and publishes ggen Docker images to Docker Hub
#
# Prerequisites:
# 1. Docker daemon must be running
# 2. Docker Hub account: seanchatman
# 3. Prebuilt binary at target/release/ggen

set -e

VERSION="5.0.0"
DOCKER_USER="seanchatman"
IMAGE_NAME="ggen"

echo "=== ggen v${VERSION} Docker Hub Deployment ==="
echo ""

# Check Docker is running
if ! docker info > /dev/null 2>&1; then
    echo "❌ Error: Docker daemon is not running"
    echo "   Please start Docker Desktop and try again"
    exit 1
fi

# Check binary exists
if [ ! -f "target/release/ggen" ]; then
    echo "❌ Error: Binary not found at target/release/ggen"
    echo "   Run: cargo build --release --bin ggen --manifest-path crates/ggen-cli/Cargo.toml"
    exit 1
fi

echo "✅ Docker daemon is running"
echo "✅ Binary found ($(ls -lh target/release/ggen | awk '{print $5}'))"
echo ""

# Login to Docker Hub
echo "🔐 Logging in to Docker Hub..."
docker login

echo ""
echo "📦 Building Docker images..."
echo "   - Using Dockerfile.binary (fast build with prebuilt binary)"
echo "   - Tags: ${DOCKER_USER}/${IMAGE_NAME}:${VERSION}, ${DOCKER_USER}/${IMAGE_NAME}:latest"
echo ""

# Build using prebuilt binary (fast method)
docker build \
  -f Dockerfile.binary \
  -t ${DOCKER_USER}/${IMAGE_NAME}:${VERSION} \
  -t ${DOCKER_USER}/${IMAGE_NAME}:latest \
  .

echo ""
echo "✅ Build complete!"
echo ""

# Verify the image
echo "🔍 Verifying image..."
docker run --rm ${DOCKER_USER}/${IMAGE_NAME}:${VERSION} --version

echo ""
echo "📤 Pushing to Docker Hub..."
echo ""

# Push versioned tag
echo "   Pushing ${DOCKER_USER}/${IMAGE_NAME}:${VERSION}..."
docker push ${DOCKER_USER}/${IMAGE_NAME}:${VERSION}

# Push latest tag
echo "   Pushing ${DOCKER_USER}/${IMAGE_NAME}:latest..."
docker push ${DOCKER_USER}/${IMAGE_NAME}:latest

echo ""
echo "✅ Deployment complete!"
echo ""
echo "📋 Verification:"
echo "   docker pull ${DOCKER_USER}/${IMAGE_NAME}:${VERSION}"
echo "   docker run --rm ${DOCKER_USER}/${IMAGE_NAME}:${VERSION} --version"
echo "   docker run --rm -v \$(pwd):/workspace ${DOCKER_USER}/${IMAGE_NAME}:${VERSION} sync"
echo ""
echo "🔗 Docker Hub: https://hub.docker.com/r/${DOCKER_USER}/${IMAGE_NAME}"
echo ""

# Optional: Multi-platform build instructions
echo "💡 For multi-platform builds (amd64 + arm64):"
echo ""
echo "   docker buildx create --use"
echo "   docker buildx build \\"
echo "     --platform linux/amd64,linux/arm64 \\"
echo "     -t ${DOCKER_USER}/${IMAGE_NAME}:${VERSION} \\"
echo "     -t ${DOCKER_USER}/${IMAGE_NAME}:latest \\"
echo "     --push \\"
echo "     ."
echo ""
