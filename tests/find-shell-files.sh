#!/bin/bash

set -eu

git ls-files | LC_ALL=C sort | while read -r line; do
    if [[ "${line}" != *'git-completion.bash' ]]; then
        HEAD="$(head -n 1 "${line}")"
        if [[ "${line}" == *'.sh' ]] || [[ "${line}" == *'.bash' ]] ||
            [[ "${line}" == *'.bats' ]] ||
            [ "${HEAD}" = '#!/bin/sh' ] || [ "${HEAD}" = '#!/bin/bash' ]; then
            echo "${line}"
        fi
    fi
done
