# shellcheck shell=bash

function htop() {
    local RETVAL
    command -v emacs-term-cmd &>/dev/null && emacs-term-cmd 'term-pager-on'
    command htop "${@}"
    RETVAL="${?}"
    command -v emacs-term-cmd &>/dev/null && emacs-term-cmd 'term-pager-off'
    return "${RETVAL}"
}
