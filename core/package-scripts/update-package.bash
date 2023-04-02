source "${DOTFILES_PACKAGE_SCRIPTS}/setup-common.bash" &&
source "${DOTFILES_PACKAGE_SCRIPTS}/common-funcs.sh" || exit 1

function update-package() {
    if [ -z "${1}" ]; then
        dotfiles-echo-red 'No argument provided to update-package.'
        return 1
    fi

    if grep "^${1}\$" "${UPDATED_ALREADY_FILE}" &>/dev/null; then
        return 0
    fi

    package-setup-vars "${1}"
    if [ -e "${PACKAGE_SETUP_FILE}" ] && ! dotfiles-package-ignored "${PACKAGE_NAME}" && dotfiles-package-installed "${PACKAGE_NAME}"; then
        if ! cd "${PACKAGE_SOURCE_DIR}"; then
            dotfiles-echo-red "Could not enter configuration directory for '${PACKAGE_NAME}'."
            package-cleanup
            return 1
        fi

        source "${PACKAGE_SETUP_FILE}"

        if [ -d "${PACKAGE_INSTALL_DIR}" ]; then
            if ! cd "${PACKAGE_INSTALL_DIR}"; then
                dotfiles-echo-red "Could not enter install directory for '${PACKAGE_NAME}'."
                package-cleanup
                return 1
            fi
        fi

        if type _update &>/dev/null; then
            if ! _update; then
                dotfiles-echo-red "Package '${PACKAGE_NAME}' failed to update; quitting."
                package-cleanup
                return 1
            fi
            touch "${DOTFILES_NEEDS_LOGOUT}"
        fi
    fi

    echo "${PACKAGE_NAME}" >> "${UPDATED_ALREADY_FILE}"

    package-cleanup
}

function update-package-root() {
    PACKAGE_ROOT="${1}"

    if [ -d "${PACKAGE_ROOT}" ]; then
        exec 3< <(ls -1 "${PACKAGE_ROOT}") || return 1
        while read -r -u 3 line; do
            update-package "${line}" || return 1
        done
    fi
}

function update-all-packages() {
    touch "${UPDATED_ALREADY_FILE}" &&
    packagerootloop update-package-root &&
    date '+%s' > "${UPDATE_FILE}" &&
    rm "${UPDATED_ALREADY_FILE}"
}
