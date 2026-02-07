#!/bin/sh
# Train CNN upscaler model from PNG files in data/ subdirectory
# Usage: ./train.sh [2x|4x] [srgb|linear] [--gpu]

set -e

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
cd "$SCRIPT_DIR"

# Defaults
FACTOR=2
COLORSPACE=srgb
USE_GPU=0

# Parse arguments (order-independent)
for arg in "$@"; do
    case "$arg" in
        2x|2)      FACTOR=2 ;;
        4x|4)      FACTOR=4 ;;
        srgb)      COLORSPACE=srgb ;;
        linear)    COLORSPACE=linear ;;
        --gpu|gpu) USE_GPU=1 ;;
        -h|--help)
            echo "Usage: $0 [2x|4x] [srgb|linear] [--gpu]"
            echo ""
            echo "  2x|4x       Upscale factor (default: 2x)"
            echo "  srgb|linear Color space (default: srgb)"
            echo "  --gpu       Use Vulkan compute backend"
            exit 0 ;;
        *)
            echo "Unknown argument: $arg"
            echo "Usage: $0 [2x|4x] [srgb|linear] [--gpu]"
            exit 1 ;;
    esac
done

SUFFIX="${FACTOR}x_${COLORSPACE}"
MODEL="model_${SUFFIX}.bin"

# Build if needed
BUILD_ARGS="CC=${CC:-gcc} OPENMP=1"
if [ "$USE_GPU" -eq 1 ]; then
    BUILD_ARGS="$BUILD_ARGS VULKAN=1"
fi

if [ ! -f ./upscaler ] || [ ./upscaler -ot main.c ] || [ ./upscaler -ot cnn.c ] || [ ./upscaler -ot image.c ] || [ ./upscaler -ot export.c ] || [ ./upscaler -ot vk_backend.c ] || [ ./upscaler -ot vk_cnn.c ]; then
    echo "Building upscaler ($BUILD_ARGS)..."
    make $BUILD_ARGS
fi

# Check data directory
if [ ! -d data ] || [ -z "$(ls data/*.png 2>/dev/null)" ]; then
    echo "ERROR: No PNG files found in $SCRIPT_DIR/data/"
    echo "Place your ground-truth (full resolution) PNG images in the data/ directory."
    exit 1
fi

echo "=== Training ${FACTOR}x upscaler (${COLORSPACE}$([ "$USE_GPU" -eq 1 ] && echo ', Vulkan GPU')) ==="
echo "Model output: ${MODEL}"
echo ""

GPU_FLAG=""
[ "$USE_GPU" -eq 1 ] && GPU_FLAG="--gpu"

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
    --output "$MODEL" \
    $GPU_FLAG

echo ""
echo "=== Training complete ==="
echo "Model saved to: $SCRIPT_DIR/$MODEL"
echo ""
echo "Export to GLSL:"
echo "  ./upscaler export --model $MODEL --format glsl --output upscaler_weights_${SUFFIX}.glsl"
echo ""
echo "Run inference:"
echo "  ./upscaler infer --model $MODEL --input test.png --output test_hr.png${GPU_FLAG:+ $GPU_FLAG}"
