#!/bin/bash
# Install runsc in Colima VM

ARCH=$(uname -m)
if [ "$ARCH" = "aarch64" ]; then
    SUFFIX="arm64"
else
    SUFFIX="amd64"
fi

echo "Installing runsc for architecture: $SUFFIX"

# Try GitHub releases first
GITHUB_RELEASE=$(curl -s https://api.github.com/repos/google/gvisor/releases/latest 2>/dev/null)
if [ -n "$GITHUB_RELEASE" ]; then
    DOWNLOAD_URL=$(echo "$GITHUB_RELEASE" | grep -o "https://github.com/google/gvisor/releases/download/[^\"]*runsc[^\"]*${SUFFIX}[^\"]*" | head -1)
    if [ -n "$DOWNLOAD_URL" ]; then
        echo "Downloading from GitHub: $DOWNLOAD_URL"
        curl -fsSL "$DOWNLOAD_URL" -o /tmp/runsc && sudo mv /tmp/runsc /usr/local/bin/runsc && sudo chmod +x /usr/local/bin/runsc && /usr/local/bin/runsc --version && exit 0
    fi
fi

# Try storage URLs with different versions
for VERSION in "20241218.0" "20241118.0" "20241018.0"; do
    URL="https://storage.googleapis.com/gvisor/releases/release/${VERSION}/${SUFFIX}/runsc"
    echo "Trying: $URL"
    if curl -fsSL "$URL" -o /tmp/runsc 2>/dev/null; then
        sudo mv /tmp/runsc /usr/local/bin/runsc
        sudo chmod +x /usr/local/bin/runsc
        if /usr/local/bin/runsc --version; then
            echo "✅ Installed runsc version $VERSION"
            exit 0
        fi
    fi
done

echo "❌ All download methods failed"
exit 1

