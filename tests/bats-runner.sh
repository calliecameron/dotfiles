#!/bin/bash

BATS_LIB_PATH="$(readlink -f "$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)/../node_modules")"
export BATS_LIB_PATH

npx bats "${@}"
