# shellcheck disable=SC2317
# Must work in bash and zsh

source "${DOTFILES_PACKAGE_SCRIPTS}/load-package-env.sh"
source "${DOTFILES_PACKAGE_SCRIPTS}/common-funcs.sh"

function load-aliases() {
    test ! -z "${DOTFILES_PROFILING}" && printf "package %s %s " "${@}" && date --rfc-3339=ns
    local PACKAGE_ROOT="${1}"
    local PACKAGE_NAME="${2}"
    local ALIAS_SHELL="${3}"
    local PACKAGE_SOURCE_DIR="${PACKAGE_ROOT}/${PACKAGE_NAME}"
    # shellcheck disable=SC2034
    local PACKAGE_INSTALL_DIR="${DOTFILES_PACKAGE_INSTALL_DIR}/${PACKAGE_NAME}"

    if [ "${ALIAS_SHELL}" = 'zsh' ] && [ -d "${PACKAGE_SOURCE_DIR}/zsh-completions" ]; then
        # shellcheck disable=SC2206
        fpath=("${PACKAGE_SOURCE_DIR}/zsh-completions" $fpath)
    fi

    local FILE="${PACKAGE_SOURCE_DIR}/aliases.${ALIAS_SHELL}"
    if [ -e "${FILE}" ]; then
        local ORIGINAL_WD="${PWD}"
        cd "${PACKAGE_SOURCE_DIR}" || return 1
        source "${FILE}"
        cd "${ORIGINAL_WD}" || return 1
    fi

    test ! -z "${DOTFILES_PROFILING}" && printf "package %s %s " "${PACKAGE_NAME}" "${ALIAS_SHELL}" && date --rfc-3339=ns
    return 0
}

function load-package-aliases() {
    local PACKAGE_ROOT="${1}"
    local PACKAGE_NAME="${2}"
    local PACKAGE_SOURCE_DIR="${PACKAGE_ROOT}/${PACKAGE_NAME}"

    if [ -d "${PACKAGE_SOURCE_DIR}" ] &&
        ! dotfiles-in-list "${DOTFILES_PACKAGES_LOADED_ALIASES}" "${PACKAGE_NAME}" &&
        ! dotfiles-package-ignored "${PACKAGE_NAME}"; then

        if (dotfiles-package-installed "${PACKAGE_NAME}" ||
            ! dotfiles-package-has-installer "${PACKAGE_SOURCE_DIR}"); then
            # Depending on invocation, the package might not have been enved yet
            if loadpackageenv "${PACKAGE_ROOT}" "${PACKAGE_NAME}" &&
                load-aliases "${PACKAGE_ROOT}" "${PACKAGE_NAME}" 'sh' &&
                load-aliases "${PACKAGE_ROOT}" "${PACKAGE_NAME}" "${DOTFILES_SHELL}"; then
                DOTFILES_PACKAGES_LOADED_ALIASES="${DOTFILES_PACKAGES_LOADED_ALIASES}:${PACKAGE_NAME}"
            else
                dotfiles-log-package-problem "Failed to load aliases for package '${PACKAGE_NAME}'."
            fi
        elif (! dotfiles-package-installed "${PACKAGE_NAME}" &&
            dotfiles-package-can-install "${PACKAGE_SOURCE_DIR}"); then
            PACKAGES_AVAILABLE='t'
        fi
    fi
}

function check-for-available() {
    if [ -n "${PACKAGES_AVAILABLE}" ]; then
        dotfiles-echo-blue "Packages are available to install; run 'dotfiles-package-list'."
    fi
}

function check-for-updates() {
    if [ -f "${DOTFILES_PACKAGE_LAST_UPDATE_FILE}" ]; then
        local LAST_UPDATE
        LAST_UPDATE="$(cat "${DOTFILES_PACKAGE_LAST_UPDATE_FILE}")"
        local NOW
        NOW="$(date '+%s')"
        local DIFF=$((NOW - LAST_UPDATE))
        local TARGET='2419200' # Seconds in four weeks

        if [ "${DIFF}" -ge "${TARGET}" ]; then
            dotfiles-echo-blue "It's been a while since packages were checked for updates; run 'dotfiles-package-update'."
        fi
    fi
}

packageloop 'Aliases' load-package-aliases

check-for-available
check-for-updates

unset -v PACKAGES_AVAILABLE
unset -f load-aliases load-package-aliases check-for-available check-for-updates
commonfuncscleanup
loadpackageenvcleanup
