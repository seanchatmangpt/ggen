#!/bin/bash
# Install runsc using multiple fallback methods

set -euo pipefail

ARCH=$(uname -m)
if [ "$ARCH" = "aarch64" ] || [ "$ARCH" = "arm64" ]; then
    SUFFIX="arm64"
else
    SUFFIX="amd64"
fi

echo "Installing runsc for architecture: $SUFFIX"

# Method 1: Try direct download from storage.googleapis.com with multiple versions
for VERSION in "20241218.0" "20241118.0" "20241018.0" "latest"; do
    URL="https://storage.googleapis.com/gvisor/releases/release/${VERSION}/${SUFFIX}/runsc"
    echo "Trying: $URL"
    if curl -fsSL "$URL" -o /tmp/runsc 2>/dev/null && [ -s /tmp/runsc ]; then
        chmod +x /tmp/runsc
        if /tmp/runsc --version > /dev/null 2>&1; then
            echo "✅ Downloaded runsc version $VERSION"
            sudo mv /tmp/runsc /usr/local/bin/runsc
            sudo chmod +x /usr/local/bin/runsc
            /usr/local/bin/runsc --version
            exit 0
        fi
    fi
done

# Method 2: Try GitHub releases API
echo "Trying GitHub releases..."
GITHUB_RELEASES=$(curl -s https://api.github.com/repos/google/gvisor/releases/latest)
if [ -n "$GITHUB_RELEASES" ]; then
    # Try to find arm64 binary
    DOWNLOAD_URL=$(echo "$GITHUB_RELEASES" | grep -o '"browser_download_url": "[^"]*runsc[^"]*' | grep -i "$SUFFIX" | head -1 | cut -d'"' -f4)
    if [ -n "$DOWNLOAD_URL" ]; then
        echo "Downloading from GitHub: $DOWNLOAD_URL"
        if curl -fsSL "$DOWNLOAD_URL" -o /tmp/runsc && [ -s /tmp/runsc ]; then
            chmod +x /tmp/runsc
            if /tmp/runsc --version > /dev/null 2>&1; then
                echo "✅ Downloaded runsc from GitHub"
                sudo mv /tmp/runsc /usr/local/bin/runsc
                sudo chmod +x /usr/local/bin/runsc
                /usr/local/bin/runsc --version
                exit 0
            fi
        fi
    fi
fi

echo "❌ All download methods failed"
exit 1

