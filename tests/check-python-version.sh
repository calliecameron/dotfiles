#!/bin/bash

set -eu

THIS_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

SYSTEM="$(/usr/bin/python3 --version | grep -E -o '3\.[0-9]+\.[0-9]+')"
LOCAL="$(cat "${THIS_DIR}/../.python-version")"

if [ "${LOCAL}" != "${SYSTEM}" ]; then
    echo "Local python version (${LOCAL}) must be the same as the system python version (${SYSTEM})"
    exit 1
fi
