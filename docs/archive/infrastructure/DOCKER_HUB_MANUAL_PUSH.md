# Docker Hub Manual Push Instructions

## Current Status

✅ **Docker image built successfully**:
- Image ID: `2c0e3fb3cca2`
- Size: 169MB
- Tags: `seanchatman/ggen:5.0.2`, `seanchatman/ggen:latest`
- Verified working: `cli 5.3.4`

⚠️ **Authentication Required**: Manual Docker Hub login needed to push

## Quick Push (3 Commands)

```bash
# 1. Login to Docker Hub
docker login --username seanchatman

# 2. Push version tag
docker push seanchatman/ggen:5.0.2

# 3. Push latest tag
docker push seanchatman/ggen:latest
```

## Detailed Steps

### Step 1: Verify Image Exists Locally

```bash
docker images seanchatman/ggen
```

**Expected output**:
```
REPOSITORY         TAG       IMAGE ID       CREATED         SIZE
seanchatman/ggen   5.0.0     2c0e3fb3cca2   X minutes ago   169MB
seanchatman/ggen   latest    2c0e3fb3cca2   X minutes ago   169MB
```

### Step 2: Verify Image Works

```bash
docker run --rm seanchatman/ggen:5.0.2 ggen --version
```

**Expected output**:
```
cli 5.3.4
```

### Step 3: Login to Docker Hub

```bash
docker login --username seanchatman
```

You'll be prompted for your Docker Hub password or access token.

**Successful login**:
```
Login Succeeded
```

### Step 4: Push to Docker Hub

Push both tags (5.0.0 and latest):

```bash
docker push seanchatman/ggen:5.0.2
docker push seanchatman/ggen:latest
```

**Expected output**:
```
The push refers to repository [docker.io/seanchatman/ggen]
8a4a7306158c: Pushed
c2ba8ff4ecb9: Pushed
5e8f05b1e451: Pushed
a2212fc2f3bd: Pushed
8b8693bf7bcf: Pushed
4f4fb700ef54: Pushed
5.0.0: digest: sha256:xxxxx size: 1234
latest: digest: sha256:xxxxx size: 1234
```

### Step 5: Verify Deployment

Pull from Docker Hub to verify:

```bash
docker pull seanchatman/ggen:5.0.2
docker run --rm seanchatman/ggen:5.0.2 ggen --version
```

Check Docker Hub web UI:
https://hub.docker.com/r/seanchatman/ggen

## Troubleshooting

### "repository does not exist or may require authorization"

**Cause**: Repository doesn't exist on Docker Hub yet, or authentication failed.

**Solution**:
1. Create repository manually at https://hub.docker.com/repository/create
2. Repository name: `ggen`
3. Visibility: Public
4. Then retry the push

### "unauthorized: incorrect username or password"

**Cause**: Login credentials incorrect or expired.

**Solution**:
1. Generate access token at https://hub.docker.com/settings/security
2. Use token as password: `docker login -u seanchatman -p YOUR_ACCESS_TOKEN`

### "denied: requested access to the resource is denied"

**Cause**: Logged in as different user than image tag.

**Solution**:
1. Verify logged in user: `cat ~/.docker/config.json | jq -r '.auths."https://index.docker.io/v1/"'`
2. Ensure username matches image tag: `seanchatman`

## After Successful Push

### Update Documentation

1. Update `DEPLOYMENT_STATUS.md`:
   ```diff
   - | **Docker Hub** | ⏳ READY | ... |
   + | **Docker Hub** | ✅ LIVE | ... |
   ```

2. Commit changes:
   ```bash
   git add DEPLOYMENT_STATUS.md DOCKER_HUB_MANUAL_PUSH.md
   git commit -m "feat(docker): Complete Docker Hub deployment for v5.0.0"
   git push origin master
   ```

### Verify Public Access

Test that anyone can pull the image:

```bash
# From a different machine or after `docker rmi seanchatman/ggen:5.0.2`
docker pull seanchatman/ggen:5.0.2
docker run --rm -v $(pwd):/workspace seanchatman/ggen:5.0.2 sync
```

## Build Information

- **Base image**: `rust:bookworm` (builder), `debian:bookworm-slim` (production)
- **Build time**: 2m 27s
- **Dependencies compiled**: 584 crates
- **Binary size**: 12MB
- **Image size**: 169MB
- **Architecture**: linux/arm64 (current build)

For multi-platform builds (amd64 + arm64), see `DOCKER.md`.

## Quick Reference

```bash
# Full deployment in 4 commands
docker login --username seanchatman
docker push seanchatman/ggen:5.0.2
docker push seanchatman/ggen:latest
docker pull seanchatman/ggen:5.0.2  # Verify
```

---

**Status**: Ready to push - image built and verified locally ✅
