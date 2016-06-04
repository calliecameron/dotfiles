# Functions available to env and alias scripts. Must be sh compatible.

message() {
    echo "${@}" >> "${DOTFILES_PACKAGE_MESSAGES_FILE}"
}

problem() {
    echo "${@}" >> "${DOTFILES_PACKAGE_PROBLEMS_FILE}"
}

homelink() {
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
    # shellcheck disable=SC2039
    local VAL
    eval VAL="\${${1}}"
    if [ -z "${VAL}" ]; then
        problem "${2} not set; use ${1} to set it in ${DOTFILES_LOCAL_VARIABLES}."
    fi
}

packagecommonfuncscleanup() {
    unset -f message problem homelink homebinlink complainunset packagecommonfuncscleanup
}
