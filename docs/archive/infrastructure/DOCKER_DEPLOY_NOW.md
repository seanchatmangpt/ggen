<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->
**Table of Contents**

- [Deploy ggen v5.0.0 to Docker Hub - Manual Steps](#deploy-ggen-v500-to-docker-hub---manual-steps)
  - [Prerequisites Check](#prerequisites-check)
  - [Step 1: Login to Docker Hub](#step-1-login-to-docker-hub)
  - [Step 2: Build the Docker Image](#step-2-build-the-docker-image)
  - [Step 3: Verify the Build Works](#step-3-verify-the-build-works)
  - [Step 4: Push Version 5.0.0 Tag](#step-4-push-version-500-tag)
  - [Step 5: Push Latest Tag](#step-5-push-latest-tag)
  - [Step 6: Verify Deployment](#step-6-verify-deployment)
  - [Step 7: Test from Docker Hub](#step-7-test-from-docker-hub)
  - [🎉 Success!](#-success)
  - [Troubleshooting](#troubleshooting)
    - [If `docker login` hangs:](#if-docker-login-hangs)
    - [If build fails with "Cannot connect to Docker daemon":](#if-build-fails-with-cannot-connect-to-docker-daemon)
    - [If push fails with "unauthorized":](#if-push-fails-with-unauthorized)
    - [If you get "denied: requested access to the resource is denied":](#if-you-get-denied-requested-access-to-the-resource-is-denied)
  - [Quick Copy-Paste (All Commands)](#quick-copy-paste-all-commands)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

# Deploy ggen v5.0.0 to Docker Hub - Manual Steps

## Prerequisites Check
✅ Docker Desktop is running (green icon in menu bar)
✅ You're in the ggen directory: `/Users/sac/ggen`
✅ Binary exists at: `target/release/ggen` (12MB)

## Step 1: Login to Docker Hub

Open your terminal and run:

```bash
docker login
```

**Enter when prompted:**
- Username: `seanchatman`
- Password: [your Docker Hub password]

**Expected output:**
```
Login Succeeded
```

## Step 2: Build the Docker Image

```bash
docker build -f Dockerfile.binary -t seanchatman/ggen:5.0.2 -t seanchatman/ggen:latest .
```

**What this does:**
- Uses `Dockerfile.binary` (fast build with prebuilt binary)
- Creates two tags: `5.0.0` and `latest`
- Takes ~30 seconds

**Expected output:**
```
[+] Building 30.2s (8/8) FINISHED
Successfully tagged seanchatman/ggen:5.0.2
Successfully tagged seanchatman/ggen:latest
```

## Step 3: Verify the Build Works

```bash
docker run --rm seanchatman/ggen:5.0.2 --version
```

**Expected output:**
```
ggen 5.0.0
```

## Step 4: Push Version 5.0.0 Tag

```bash
docker push seanchatman/ggen:5.0.2
```

**Expected output:**
```
The push refers to repository [docker.io/seanchatman/ggen]
5f70bf18a086: Pushed
...
5.0.0: digest: sha256:xxxxx size: 1234
```

## Step 5: Push Latest Tag

```bash
docker push seanchatman/ggen:latest
```

**Expected output:**
```
The push refers to repository [docker.io/seanchatman/ggen]
5f70bf18a086: Layer already exists
...
latest: digest: sha256:xxxxx size: 1234
```

## Step 6: Verify Deployment

Visit in browser: **https://hub.docker.com/r/seanchatman/ggen**

You should see:
- ✅ Tag: `5.0.0`
- ✅ Tag: `latest`
- ✅ Last pushed: [today's date]

## Step 7: Test from Docker Hub

```bash
docker pull seanchatman/ggen:5.0.2
docker run --rm seanchatman/ggen:5.0.2 --help
```

## 🎉 Success!

ggen v5.0.0 is now live on all 4 distribution channels:

1. ✅ **crates.io**: `cargo install ggen-cli-lib`
2. ✅ **Homebrew**: `brew install seanchatmangpt/ggen/ggen`
3. ✅ **GitHub**: https://github.com/seanchatmangpt/ggen/releases/tag/v5.0.0
4. ✅ **Docker Hub**: `docker pull seanchatman/ggen:5.0.2`

---

## Troubleshooting

### If `docker login` hangs:
1. Press Ctrl+C
2. Click Docker Desktop menu bar icon → Restart
3. Wait for green icon
4. Try again

### If build fails with "Cannot connect to Docker daemon":
1. Open Docker Desktop app manually
2. Wait for green icon in menu bar
3. Try build command again

### If push fails with "unauthorized":
```bash
docker logout
docker login
# Re-enter credentials
```

### If you get "denied: requested access to the resource is denied":
- Verify username is exactly: `seanchatman` (no typos)
- Verify you have push access to the repository

---

## Quick Copy-Paste (All Commands)

```bash
# Login
docker login

# Build
docker build -f Dockerfile.binary -t seanchatman/ggen:5.0.2 -t seanchatman/ggen:latest .

# Verify
docker run --rm seanchatman/ggen:5.0.2 --version

# Push both tags
docker push seanchatman/ggen:5.0.2
docker push seanchatman/ggen:latest

# Test pull
docker pull seanchatman/ggen:5.0.2
```

**Total time:** ~3 minutes
