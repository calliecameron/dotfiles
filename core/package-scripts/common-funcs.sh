# Functions available to env, alias and setup.bash scripts. Must be sh compatible.

packagerootloop() {
    # Not intended for use by packages, but rather for package-loading
    # scripts. Call arg with each package root as argument.
    # shellcheck disable=SC2039
    local ORIGINAL_IFS
    ORIGINAL_IFS="${IFS}"
    IFS=':'
    test ! -z "${ZSH_VERSION}" && setopt sh_word_split

    for PACKAGE_ROOT in ${DOTFILES_PACKAGE_ROOTS}; do
        IFS="${ORIGINAL_IFS}"
        test ! -z "${ZSH_VERSION}" && unsetopt sh_word_split

        if ! "${1}" "${PACKAGE_ROOT}"; then
            return 1
        fi

        test ! -z "${ZSH_VERSION}" && setopt sh_word_split
        IFS=':'
    done

    test ! -z "${ZSH_VERSION}" && unsetopt sh_word_split
    IFS="${ORIGINAL_IFS}"
    return 0
}

homebinlink() {
    # Ensure command in the first argument (must be on the PATH) is
    # symlinked in ~/.bin (which always has the highest priority on
    # the PATH) with the names given as the remaining arguments.

    # Args: command link [links...]
    # shellcheck disable=SC2039
    local COMMAND FULL_COMMAND LINKNAME
    COMMAND="${1}"
    shift

    if which "${COMMAND}" >/dev/null; then
        FULL_COMMAND="$(which "${COMMAND}")"
        if ! mkdir -p "${DOTFILES_LOCAL_BIN}"; then
            dotfiles-log-package-problem "Cannot create '${DOTFILES_LOCAL_BIN}'"
            return 1
        fi

        while [ ! -z "${1}" ]; do
            LINKNAME="${1}"
            if [ -e "${DOTFILES_LOCAL_BIN}/${LINKNAME}" ]; then
                if [ ! -h "${DOTFILES_LOCAL_BIN}/${LINKNAME}" ]; then
                    dotfiles-log-package-problem "'${COMMAND}' link '${DOTFILES_LOCAL_BIN}/${LINKNAME}' exists but is not a link to '${COMMAND}'."
                fi
            else
                ln -s "${FULL_COMMAND}" "${DOTFILES_LOCAL_BIN}/${LINKNAME}" || dotfiles-log-package-problem "Cannot create link '${DOTFILES_LOCAL_BIN}/${LINKNAME}' to '${COMMAND}'."
            fi

            shift
        done
    fi
    return 0
}

complainunset() {
    # Display a problem message if the variable name passed in as the
    # first argument is unset; use the second argument as a
    # human-readable description.

    # shellcheck disable=SC2039
    local VAL
    eval VAL="\${${1}}"
    if [ -z "${VAL}" ]; then
        dotfiles-log-package-problem "${2} not set; use ${1} to set it in ${DOTFILES_LOCAL_VARIABLES}."
    fi
}

commonfuncscleanup() {
    unset -f packagerootloop homebinlink complainunset commonfuncscleanup
}
