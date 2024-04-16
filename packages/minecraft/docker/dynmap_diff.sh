#!/bin/bash

set -eu

function usage() {
    echo "Usage: $(basename "${0}") old_instance new_instance"
    exit 1
}

test -z "${1:-}" && usage
test -d "${1}" || usage
test -d "${1}/dynmap"
OLD="$(readlink -f "${1}/dynmap")"
test -z "${2:-}" && usage
test -d "${2}" || usage
test -d "${2}/dynmap"
NEW="$(readlink -f "${2}/dynmap")"

cd "${OLD}"

find . -type f | LC_ALL=C sort | while read -r config; do
    NEW_CONFIG="${NEW}/${config}"
    if [ -f "${NEW_CONFIG}" ]; then
        NEW_CONFIG="$(readlink -f "${NEW_CONFIG}")"
    else
        NEW_CONFIG='/dev/null'
    fi

    echo "${config}"
    echo
    diff "${NEW_CONFIG}" "${config}" || true
    echo
    echo
done
