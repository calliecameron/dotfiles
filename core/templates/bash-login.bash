# shellcheck shell=bash

if [ -f "${HOME}/.profile" ]; then
    # shellcheck source=/dev/null
    source "${HOME}/.profile"
fi
