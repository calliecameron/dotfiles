#!/bin/bash

set -eu

git ls-files | LC_ALL=C sort | while read -r line; do
    if [[ "${line}" == *'.bats' ]]; then
        echo "${line}"
    fi
done
