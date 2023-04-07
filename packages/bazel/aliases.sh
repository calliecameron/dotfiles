# shellcheck shell=bash

function bazel-workspace-root() {
    local DIR
    DIR="$(readlink -f .)"

    while true; do
        if [ -f "${DIR}/WORKSPACE" ]; then
            echo "${DIR}"
            return 0
        elif [ "${DIR}" = '/' ]; then
            return 0
        else
            DIR="$(readlink -f "${DIR}/..")"
        fi
    done
}

function bazel-workspace-check() {
    local ROOT
    ROOT="$(bazel-workspace-root)"

    if [ -n "${ROOT}" ]; then
        bo="${ROOT}/bazel-bin/$(realpath "--relative-to=${ROOT}" .)"
    else
        unset bo
    fi
}

function bazel-workspace-output() {
    if [ -n "${bo}" ]; then
        cd "${bo}" || return 1
    else
        echo 'Not in a bazel workspace'
        return 1
    fi
}

alias b='bazel'
alias bb='b build'
alias br='b run'
alias bt='b test'
alias bo='bazel-workspace-output'

bazel-workspace-check
