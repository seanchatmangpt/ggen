#!/bin/bash
set -e

echo "=== Running Chicago TDD Test Suite ==="

# Rust tests
echo "Running Rust CLI tests..."
cd rust-test-project
cargo test --quiet
cd ..

# TypeScript tests
echo "Running TypeScript CLI tests..."
cd typescript-test-project
npm test --silent
cd ..

# Python tests
echo "Running Python CLI tests..."
cd python-test-project
pytest -q
cd ..

echo "=== All tests passed! ==="
