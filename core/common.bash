# This file should be included with 'source', not executed directly

# Dir of the including script, not of this file (which would be BASH_SOURCE[0]).
# Doesn't work if this file is sourced while in a function (rather than a
# top-level script) -- but that should never be done anyway!
# shellcheck disable=SC2034
THIS_DIR="$(cd "$(dirname "${BASH_SOURCE[1]}")" && pwd)"

function echo-colour() {
    echo -e "\e[${1}m${2}\e[0m"
}

function echo-red() {
    echo-colour '31' "${1}"
}

function echo-green() {
    echo-colour '32' "${1}"
}

function echo-yellow() {
    echo-colour '33' "${1}"
}

function echo-blue() {
    echo-colour '34' "${1}"
}

function yn-y() {
    # Y is the default
    local REPLY
    read -p "${1} [Y/n] " -r
    if [[ $REPLY =~ ^[Nn]$ ]]; then
        return 1
    else
        return 0
    fi
}

function yn-n() {
    # N is the default
    local REPLY
    read -p "${1} [y/N] " -r
    if [[ $REPLY =~ ^[Yy]$ ]]; then
        return 0
    else
        return 1
    fi
}

function valid-protocol() {
    if [ "${1}" = 'tcp' ] || [ "${1}" = 'udp' ]; then
        return 0
    else
        return 1
    fi
}

function valid-port() {
    if [ -n "${1}" ]; then
        if [[ "${1}" != *[!0-9]* ]]; then
            if (("${1}" >= 0 && "${1}" <= 65535)); then
                return 0
            fi
        fi
    fi
    return 1
}
