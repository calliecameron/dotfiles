# shellcheck shell=bash

function anki() {
    nixGLIntel "$(command -v anki)" "${@}"
}
