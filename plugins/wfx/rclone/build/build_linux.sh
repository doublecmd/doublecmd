#!/bin/bash
# Build rclone WFX plugin on Debian 12 (x86_64)
#
# Usage:
#   sudo ./build_linux.sh         Build the plugin (installs dependencies)
#   ./build_linux.sh clean        Remove build artifacts

set -e

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
PLUGIN_DIR="$(cd "$SCRIPT_DIR/.." && pwd)"
SRC_DIR="$PLUGIN_DIR/src"
OUTPUT="$PLUGIN_DIR/rclone.wfx"
LIB_DIR="$PLUGIN_DIR/lib"

# Handle clean command
if [ "$1" = "clean" ]; then
    echo "Cleaning build artifacts..."
    rm -rf "$LIB_DIR"
    rm -f "$OUTPUT"
    echo "Clean complete."
    exit 0
fi

echo "=== Installing dependencies ==="

# Update package list
apt-get update

# Install Lazarus and Free Pascal
apt-get install -y \
    lazarus \
    fpc \
    fpc-source \
    make

echo "=== Building rclone WFX plugin ==="

cd "$SRC_DIR"

lazbuild --build-mode=Release rclone.lpi

echo "=== Build complete ==="
echo "Output: $OUTPUT"

# Show file info
ls -la "$OUTPUT"
file "$OUTPUT"
