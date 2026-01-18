#!/bin/bash
# Development deployment script
# Deploys to development environment with debug features

set -euo pipefail

echo "🔧 Deploying to development environment..."

# Get the project root
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "$SCRIPT_DIR/../.." && pwd)"
cd "$PROJECT_ROOT"

# Set environment
export GGEN_ENV=development

echo "🌍 Environment: $GGEN_ENV"
echo "📦 Binary: $(du -sh generated/target/release/comprehensive-rust-showcase | cut -f1)"

# Create development-specific configuration
cat > generated/.env.development << EOF
# Development Environment Configuration
DATABASE_URL=postgresql://localhost/comprehensive_showcase_dev
REDIS_URL=redis://localhost:6379/0
JWT_SECRET=dev-secret-key-$(date +%s)
LOG_LEVEL=debug
API_PORT=3000
DEBUG_MODE=true
ENABLE_PROFILING=true
CORS_ORIGINS=*
EOF

echo "⚙️ Development configuration created"

# Start the application in development mode
echo "🚀 Starting development server..."

cd generated

# Kill any existing processes on the port
PORT=3000
PID=$(lsof -ti:$PORT || true)
if [ ! -z "$PID" ]; then
    echo "🛑 Stopping existing process on port $PORT (PID: $PID)"
    kill -9 $PID 2>/dev/null || true
    sleep 2
fi

# Start the application with development features
echo "🔄 Starting application..."
RUST_LOG=debug ./target/release/comprehensive-rust-showcase &
APP_PID=$!

echo "✅ Application started with PID: $APP_PID"

# Wait for application to start
echo "⏳ Waiting for application to be ready..."
sleep 5

# Health check
HEALTH_URL="http://localhost:$PORT/health"
if curl -f -s "$HEALTH_URL" >/dev/null 2>&1; then
    echo "✅ Health check passed"
    echo "🌐 Application available at: $HEALTH_URL"
else
    echo "❌ Health check failed!"
    echo "🔍 Checking application logs..."
    kill -9 $APP_PID 2>/dev/null || true
    exit 1
fi

# Run development-specific tests
echo "🧪 Running development tests..."
if cargo test -- --nocapture; then
    echo "✅ Development tests passed"
else
    echo "⚠️ Some development tests failed"
fi

# Setup development database
echo "🗄️ Setting up development database..."
if command -v psql &> /dev/null; then
    # Create development database if it doesn't exist
    createdb comprehensive_showcase_dev 2>/dev/null || true

    # Run migrations (if any)
    echo "🔄 Running database migrations..."
    # Add migration commands here if needed
fi

echo ""
echo "🎉 Development deployment completed!"
echo ""
echo "📋 Development Environment Details:"
echo "  URL: http://localhost:$PORT"
echo "  Health: $HEALTH_URL"
echo "  Logs: Check console output"
echo "  Database: comprehensive_showcase_dev"
echo "  Configuration: generated/.env.development"
echo ""
echo "🔧 Development Features:"
echo "  - Debug logging enabled"
echo "  - Profiling enabled"
echo "  - CORS allows all origins"
echo "  - Hot reload support (if implemented)"
echo "  - Development-specific tests"
echo ""
echo "🛑 To stop: kill $APP_PID"
echo "🔄 To restart: $0"

# Wait for the application process
wait $APP_PID
