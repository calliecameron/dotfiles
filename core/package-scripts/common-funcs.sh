# shellcheck shell=dash
# Must work in dash, bash and zsh.

packageloop() {
    # Args: msg func
    #   where func takes PACKAGE_ROOT PACKAGE as args
    local FUNC MSG ORIGINAL_IFS PACKAGE_NAME PACKAGE_ROOT TEMPFILE
    MSG="${1}"
    FUNC="${2}"
    ORIGINAL_IFS="${IFS}"
    IFS=':'
    test ! -z "${ZSH_VERSION:-}" && setopt sh_word_split

    for PACKAGE_ROOT in ${DOTFILES_PACKAGE_ROOTS}; do
        if [ -n "${PACKAGE_ROOT}" ] && [ -d "${PACKAGE_ROOT}" ]; then
            IFS="${ORIGINAL_IFS}"
            test ! -z "${ZSH_VERSION:-}" && unsetopt sh_word_split

            if ! TEMPFILE="$(mktemp)"; then
                dotfiles-log-package-problem "${MSG}, ${PACKAGE_ROOT}: unable to create temporary file."
                return 1
            fi

            if ! command ls -1 "${PACKAGE_ROOT}" | LC_ALL=C sort >"${TEMPFILE}"; then
                dotfiles-log-package-problem "${MSG}, ${PACKAGE_ROOT}: failed to list packages."
                command rm "${TEMPFILE}"
                return 1
            fi

            while read -r PACKAGE_NAME <&3; do
                if ! "${FUNC}" "${PACKAGE_ROOT}" "${PACKAGE_NAME}"; then
                    dotfiles-log-package-problem "${MSG}, ${PACKAGE_ROOT}, ${PACKAGE_NAME}: call failed."
                fi
            done 3<"${TEMPFILE}"

            command rm "${TEMPFILE}"

            test ! -z "${ZSH_VERSION:-}" && setopt sh_word_split
            IFS=':'
        fi
    done

    test ! -z "${ZSH_VERSION:-}" && unsetopt sh_word_split
    IFS="${ORIGINAL_IFS}"
    return 0
}

complainunset() {
    # Display a problem message if the variable name passed in as the first
    # argument is unset; use the second argument as a human-readable
    # description.
    local VAL
    eval VAL="\${${1}}"
    if [ -z "${VAL}" ]; then
        dotfiles-log-package-problem "${2} not set; use ${1} to set it in ${DOTFILES_LOCAL_VARIABLES}."
    fi
}

commonfuncscleanup() {
    unset -f packageloop complainunset commonfuncscleanup
}
