# Manual Docker Hub Deployment (When Docker Daemon is Slow)

## Issue
Docker daemon on macOS is unresponsive/hanging on commands. This is a known issue with Docker Desktop.

## Solution: Restart Docker Desktop

**Steps to fix:**

1. **Quit Docker Desktop completely**:
   ```bash
   # Kill Docker Desktop app
   osascript -e 'quit app "Docker"'

   # Or manually: Docker menu bar icon → Quit Docker Desktop
   ```

2. **Wait 10 seconds for cleanup**

3. **Restart Docker Desktop**:
   - Open Spotlight (Cmd+Space)
   - Type "Docker"
   - Press Enter

4. **Wait for Docker to start** (green icon in menu bar)

5. **Verify Docker is responsive**:
   ```bash
   docker version
   # Should return version info in <2 seconds
   ```

## After Docker is Responsive: Manual Deployment

Since the automated script may hang, use these manual steps instead:

### Step 1: Login to Docker Hub
```bash
docker login
# Username: seanchatman
# Password: [your Docker Hub password]
```

### Step 2: Build the Image
```bash
cd /Users/sac/ggen

# Using prebuilt binary (fast - ~30 seconds)
docker build \
  -f Dockerfile.binary \
  -t seanchatman/ggen:5.0.0 \
  -t seanchatman/ggen:latest \
  .
```

**Expected output:**
```
[+] Building 30.2s (8/8) FINISHED
 => [1/3] FROM debian:bookworm-slim
 => [2/3] RUN apt-get update && apt-get install -y ca-certificates libssl3
 => [3/3] COPY target/release/ggen /usr/local/bin/ggen
 => exporting to image
Successfully tagged seanchatman/ggen:5.0.0
Successfully tagged seanchatman/ggen:latest
```

### Step 3: Verify the Build
```bash
docker run --rm seanchatman/ggen:5.0.0 --version
```

**Expected output:**
```
ggen 5.0.0
```

### Step 4: Push to Docker Hub
```bash
# Push versioned tag
docker push seanchatman/ggen:5.0.0

# Push latest tag
docker push seanchatman/ggen:latest
```

**Expected output:**
```
The push refers to repository [docker.io/seanchatman/ggen]
5.0.0: digest: sha256:xxxxx size: 1234
latest: digest: sha256:xxxxx size: 1234
```

### Step 5: Verify Deployment
```bash
# Pull from Docker Hub
docker pull seanchatman/ggen:5.0.0

# Test it works
docker run --rm seanchatman/ggen:5.0.0 --help
```

## Alternative: Multi-Platform Build (If buildx works)

If `docker buildx` is responsive:

```bash
# Create buildx builder (one time)
docker buildx create --use --name ggen-builder

# Build and push for both amd64 and arm64
docker buildx build \
  --platform linux/amd64,linux/arm64 \
  -f Dockerfile.binary \
  -t seanchatman/ggen:5.0.0 \
  -t seanchatman/ggen:latest \
  --push \
  .
```

**Benefits:**
- Single command builds both platforms
- Automatically pushes to Docker Hub
- Users on Intel and ARM can use the same image

## Troubleshooting

### "Cannot connect to Docker daemon"
```bash
# Check Docker Desktop is running
ps aux | grep -i docker | grep -v grep

# If not running, open Docker Desktop app
open -a Docker
```

### "permission denied while trying to connect"
```bash
# Make sure you're in the docker group (should be automatic on macOS)
docker version
```

### Build hangs/freezes
```bash
# Kill the build
Ctrl+C

# Restart Docker Desktop (see steps above)

# Try again with verbose output
docker build -f Dockerfile.binary -t seanchatman/ggen:5.0.0 --progress=plain .
```

### Push fails with "unauthorized"
```bash
# Login again
docker logout
docker login
# Username: seanchatman
```

## Success Verification

After successful deployment, verify at:
- https://hub.docker.com/r/seanchatman/ggen

You should see:
- ✅ Tag: `5.0.0`
- ✅ Tag: `latest`
- ✅ Last pushed: [today's date]

Then update `DEPLOYMENT_STATUS.md` to mark Docker Hub as ✅ LIVE.

## Quick Reference

```bash
# Full deployment in 4 commands (after Docker restart):
docker login
docker build -f Dockerfile.binary -t seanchatman/ggen:5.0.0 -t seanchatman/ggen:latest .
docker push seanchatman/ggen:5.0.0
docker push seanchatman/ggen:latest
```

**Estimated time:** ~2 minutes (after Docker restart)
