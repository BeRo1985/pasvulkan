#!/bin/sh
# Train CNN upscaler model from PNG files in data/ subdirectory
# Usage: ./train.sh [2x|4x] [srgb|linear]

set -e

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
cd "$SCRIPT_DIR"

# Defaults
FACTOR=2
COLORSPACE=srgb

# Parse arguments
[ -n "$1" ] && case "$1" in
    2x|2) FACTOR=2 ;;
    4x|4) FACTOR=4 ;;
    *)    echo "Usage: $0 [2x|4x] [srgb|linear]"; exit 1 ;;
esac
[ -n "$2" ] && COLORSPACE="$2"

SUFFIX="${FACTOR}x_${COLORSPACE}"
MODEL="model_${SUFFIX}.bin"

# Build if needed
if [ ! -f ./upscaler ] || [ ./upscaler -ot main.c ] || [ ./upscaler -ot cnn.c ] || [ ./upscaler -ot image.c ] || [ ./upscaler -ot export.c ]; then
    echo "Building upscaler..."
    make CC="${CC:-gcc}" OPENMP=1
fi

# Check data directory
if [ ! -d data ] || [ -z "$(ls data/*.png 2>/dev/null)" ]; then
    echo "ERROR: No PNG files found in $SCRIPT_DIR/data/"
    echo "Place your ground-truth (full resolution) PNG images in the data/ directory."
    exit 1
fi

echo "=== Training ${FACTOR}x upscaler (${COLORSPACE}) ==="
echo "Model output: ${MODEL}"
echo ""

./upscaler train \
    --data data \
    --factor "$FACTOR" \
    --colorspace "$COLORSPACE" \
    --epochs 200 \
    --batch 16 \
    --patch 32 \
    --lr 0.001 \
    --lr-decay 100 \
    --feat1 64 \
    --feat2 32 \
    --loss l1 \
    --save-every 50 \
    --output "$MODEL"

echo ""
echo "=== Training complete ==="
echo "Model saved to: $SCRIPT_DIR/$MODEL"
echo ""
echo "Export to GLSL:"
echo "  ./upscaler export --model $MODEL --format glsl --output upscaler_weights_${SUFFIX}.glsl"
echo ""
echo "Run inference:"
echo "  ./upscaler infer --model $MODEL --input test.png --output test_hr.png"
