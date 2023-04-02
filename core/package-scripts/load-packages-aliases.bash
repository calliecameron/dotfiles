# -*- Shell-script -*-
# This script must work in bash and zsh

source "${DOTFILES_PACKAGE_SCRIPTS}/load-package-env.sh"
source "${DOTFILES_PACKAGE_SCRIPTS}/common-funcs.sh"

function doaliases() {
    # Arg: shell
    test ! -z "${DOTFILES_PROFILING}" && printf "package %s %s " "${PACKAGE_NAME}" "${1}" && date --rfc-3339=ns

    if [ "${1}" = 'zsh' ] && [ -d "${PACKAGE_SOURCE_DIR}/zsh-completions" ]; then
        fpath=("${PACKAGE_SOURCE_DIR}/zsh-completions" $fpath)
    fi

    local FILE="${PACKAGE_SOURCE_DIR}/aliases.${1}"
    if [ -e "${FILE}" ]; then
        local ORIGINAL_WD="${PWD}"
        cd "${PACKAGE_SOURCE_DIR}" || return 1
        source "${FILE}"
        cd "${ORIGINAL_WD}" || return 1
    fi

    test ! -z "${DOTFILES_PROFILING}" && printf "package %s %s " "${PACKAGE_NAME}" "${1}" && date --rfc-3339=ns
    return 0
}

function load-packages-aliases() {
    if [ ! -z "${1}" ]; then
        PACKAGE_ROOT="${1}"

        if [ -d "${PACKAGE_ROOT}" ]; then
            local TEMPFILE
            if TEMPFILE="$(mktemp)"; then
                if command ls -1 "${PACKAGE_ROOT}" > "${TEMPFILE}"; then

                    while read -r line <&3; do
                        PACKAGE_NAME="${line}"
                        PACKAGE_SOURCE_DIR="${PACKAGE_ROOT}/${PACKAGE_NAME}"
                        # shellcheck disable=SC2034
                        PACKAGE_INSTALL_DIR="${DOTFILES_PACKAGE_INSTALL_DIR}/${PACKAGE_NAME}"
                        PACKAGE_INSTALLED_FILE="${DOTFILES_PACKAGE_INSTALL_DIR}/${PACKAGE_NAME}.installed"

                        if [ -d "${PACKAGE_SOURCE_DIR}" ] &&
                               ! dotfiles-in-list "${DOTFILES_PACKAGES_LOADED_ALIASES}" "${PACKAGE_NAME}" &&
                               ! dotfiles-package-ignored "${PACKAGE_NAME}"; then
                            if [ -e "${PACKAGE_INSTALLED_FILE}" ] || [ ! -e "${PACKAGE_SOURCE_DIR}/setup.bash" ]; then

                                # Newly-installed packages won't have been enved at login time
                                loadpackageenv "${PACKAGE_NAME}" &&

                                doaliases 'sh' &&
                                doaliases "${DOTFILES_SHELL}" &&

                                # shellcheck disable=SC2015
                                DOTFILES_PACKAGES_LOADED_ALIASES="${DOTFILES_PACKAGES_LOADED_ALIASES}:${PACKAGE_NAME}" || dotfiles-log-package-problem "Failed to load aliases for package '${PACKAGE_NAME}'."
                            fi
                        fi

                        unset PACKAGE_NAME
                        unset PACKAGE_SOURCE_DIR
                        unset PACKAGE_INSTALL_DIR
                        unset PACKAGE_INSTALLED_FILE
                    done 3<"${TEMPFILE}"
                else
                    dotfiles-log-package-problem 'Could not load package aliases; failed to list packages.'
                fi

                command rm "${TEMPFILE}"
            else
                dotfiles-log-package-problem 'Could not load package aliases; failed to create temporary file.'
            fi
        fi

        unset PACKAGE_ROOT
    else
        dotfiles-log-package-problem 'Package configuration root must be specified.'
    fi
}


packagerootloop load-packages-aliases


unset -f doaliases load-packages-aliases
commonfuncscleanup
loadpackageenvcleanup
