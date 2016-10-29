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

message() {
    # Display a message in blue the next time an interactive shell is
    # started (use instead of echo, which won't be seen in env
    # scripts).
    echo "${@}" >> "${DOTFILES_PACKAGE_MESSAGES_FILE}"
}

problem() {
    # Display a message in orange the next time an interactive shell
    # is started (use instead of echo, which won't be seen in env
    # scripts).
    echo "${@}" >> "${DOTFILES_PACKAGE_PROBLEMS_FILE}"
}

homelink() {
    # Ensure the first argument is symlinked in the home directory, or
    # at another location if specified by a second argument.

    # shellcheck disable=SC2039
    local SRC DST PROBLEM
    SRC="${1}"
    if [ ! -z "${SRC}" ] && [ -e "${SRC}" ]; then
        if [ ! -z "${2}" ]; then
            DST="${2}"
            if ! mkdir -p "$(dirname "${DST}")"; then
                problem "Creating symlink directory '$(dirname "${DST}")' failed."
                return 1
            fi
        else
            if [ -d "${SRC}" ]; then
                DST="${HOME}/$(basename "${SRC}")"
            else
                DST="${HOME}/.$(basename "${SRC}")"
            fi
        fi

        PROBLEM=''
        if [ -h "${DST}" ]; then
            if [ "$(readlink -f "${DST}")" != "${SRC}" ]; then
                PROBLEM='t'
            fi
        elif [ -e "${DST}" ]; then
            PROBLEM='t'
        fi

        if [ ! -z "${PROBLEM}" ]; then
            problem "'${DST}' exists, but is not a symlink to shared file '${SRC}'; fix it manually."
            return 1
        elif [ ! -h "${DST}" ]; then
            if ! ln -s "${SRC}" "${DST}"; then
                problem "Symlinking '${SRC}' to '${DST}' failed."
                return 1
            fi
        fi
    fi
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
            problem "Cannot create '${DOTFILES_LOCAL_BIN}'"
            return 1
        fi

        while [ ! -z "${1}" ]; do
            LINKNAME="${1}"
            if [ -e "${DOTFILES_LOCAL_BIN}/${LINKNAME}" ]; then
                if [ ! -h "${DOTFILES_LOCAL_BIN}/${LINKNAME}" ]; then
                    problem "'${COMMAND}' link '${DOTFILES_LOCAL_BIN}/${LINKNAME}' exists but is not a link to '${COMMAND}'."
                fi
            else
                ln -s "${FULL_COMMAND}" "${DOTFILES_LOCAL_BIN}/${LINKNAME}" || problem "Cannot create link '${DOTFILES_LOCAL_BIN}/${LINKNAME}' to '${COMMAND}'."
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
        problem "${2} not set; use ${1} to set it in ${DOTFILES_LOCAL_VARIABLES}."
    fi
}

os() {
    while [ ! -z "${1}" ]; do
        if [ "${DOTFILES_OS}" = "${1}" ]; then
            return 0
        fi
        shift
    done
    return 1
}

commonfuncscleanup() {
    unset -f packagerootloop message problem homelink homebinlink complainunset os commonfuncscleanup
}
