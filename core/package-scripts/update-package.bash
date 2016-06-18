source "${DOTFILES_PACKAGE_SCRIPTS}/packages-common.bash" &&
source "${DOTFILES_PACKAGE_SCRIPTS}/package-common-funcs.sh" || exit 1

function update-package() {
    if [ -z "${1}" ]; then
        echo-red 'No argument provided to update-package.'
        return 1
    fi

    package-setup-vars "${1}"
    if [ -e "${PACKAGE_SETUP_FILE}" ] && package-installed; then
        if ! cd "${PACKAGE_CONF_DIR}"; then
            echo-red "Could not enter configuration directory for '${PACKAGE_NAME}'."
            package-cleanup
            return 1
        fi

        source "${PACKAGE_SETUP_FILE}"

        if [ -d "${PACKAGE_INSTALL_DIR}" ]; then
            if ! cd "${PACKAGE_INSTALL_DIR}"; then
                echo-red "Could not enter install directory for '${PACKAGE_NAME}'."
                package-cleanup
                return 1
            fi
        fi

        if type _update &>/dev/null; then
            if ! _update; then
                echo-red "Package '${PACKAGE_NAME}' failed to update; quitting."
                package-cleanup
                return 1
            fi
            touch "${DOTFILES_NEEDS_LOGOUT}"
        fi
    fi
    package-cleanup
}
