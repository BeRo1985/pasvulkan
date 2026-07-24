#!/usr/bin/env bash

set -euo pipefail

test_directory="$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd)"
lazbuild_binary="${LAZBUILD_BIN:-${HOME}/fpcupdeluxe/lazarus/lazbuild}"
lazarus_config_directory="${LAZARUS_CONFIG_DIR:-${HOME}/fpcupdeluxe/config_lazarus}"

"${lazbuild_binary}" \
 --pcp="${lazarus_config_directory}" \
 -B \
 "${test_directory}/vulkanmemorytests.lpi"

"${test_directory}/bin/vulkanmemorytests" "$@"
