function htop() {
    local RETVAL
    type emacs-term-cmd &>/dev/null && emacs-term-cmd 'term-pager-on'
    command htop "${@}"
    RETVAL="${?}"
    type emacs-term-cmd &>/dev/null && emacs-term-cmd 'term-pager-off'
    return "${RETVAL}"
}
