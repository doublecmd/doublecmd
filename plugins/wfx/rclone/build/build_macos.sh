#!/bin/bash
# Build rclone WFX plugin on macOS using Free Pascal
# Copyright (C) 2026 Miklos Mukka Szel <contact@miklos-szel.com>
#
# Usage:
#   ./build_macos.sh         Build the plugin
#   ./build_macos.sh clean   Remove build artifacts

set -e

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
PLUGIN_DIR="$(cd "$SCRIPT_DIR/.." && pwd)"
SRC_DIR="$PLUGIN_DIR/src"
SDK_DIR="$PLUGIN_DIR/../../../sdk"
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

echo "=== rclone WFX Plugin Build Script for macOS ==="

# Create lib directory
mkdir -p "$LIB_DIR"

# Method 1: Try lazbuild
if command -v lazbuild &> /dev/null; then
    echo "Found lazbuild, using Lazarus to build..."
    cd "$SRC_DIR"
    lazbuild --build-mode=Release rclone.lpi
    echo "Build complete: $OUTPUT"
    exit 0
fi

if [ -x "/Applications/Lazarus/lazbuild" ]; then
    echo "Found Lazarus in /Applications, using lazbuild..."
    cd "$SRC_DIR"
    /Applications/Lazarus/lazbuild --build-mode=Release rclone.lpi
    echo "Build complete: $OUTPUT"
    exit 0
fi

# Method 2: Try direct fpc compilation
if command -v fpc &> /dev/null; then
    echo "Found fpc, compiling directly..."
    cd "$SRC_DIR"

    fpc \
        -Mdelphi \
        -Sh \
        -O3 \
        -XX \
        -CX \
        -Fi"$SDK_DIR" \
        -Fu"$SDK_DIR" \
        -FU"$LIB_DIR" \
        -o"$OUTPUT" \
        rclone.lpr

    echo "Build complete: $OUTPUT"
    exit 0
fi

# Method 3: Offer to install FPC via Homebrew
echo ""
echo "No Free Pascal compiler found."
echo ""
echo "To install, choose one of these options:"
echo ""
echo "Option A: Install via Homebrew (recommended)"
echo "    brew install fpc"
echo "    Then run this script again."
echo ""
echo "Option B: Download Lazarus manually"
echo "    1. Go to https://www.lazarus-ide.org/index.php?page=downloads"
echo "    2. Download for macOS aarch64:"
echo "       - fpc-3.2.2.intelarm64-macosx.dmg"
echo "       - lazarus-3.6-macosx-aarch64.pkg"
echo "    3. Install both packages"
echo "    4. Run this script again"
echo ""
echo "Option C: Use GitHub Actions (no local install needed)"
echo "    1. Push this repository to GitHub"
echo "    2. Go to Actions tab"
echo "    3. Run the 'Build rclone WFX Plugin' workflow"
echo "    4. Download the artifact"
echo ""

# Ask user if they want to try Homebrew
read -p "Try to install fpc via Homebrew now? [y/N] " -n 1 -r
echo
if [[ $REPLY =~ ^[Yy]$ ]]; then
    if command -v brew &> /dev/null; then
        echo "Installing fpc..."
        brew install fpc
        echo ""
        echo "FPC installed. Running build..."
        exec "$0"  # Re-run this script
    else
        echo "Homebrew not found. Please install Homebrew first:"
        echo '    /bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"'
        exit 1
    fi
fi

exit 1
