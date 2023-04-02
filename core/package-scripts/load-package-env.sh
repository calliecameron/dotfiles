# This must work in plain old sh

loadpackageenv() {
    # shellcheck disable=SC2039
    local PACKAGE_NAME PACKAGE_CONF_DIR PACKAGE_INSTALL_DIR PACKAGE_INSTALLED_FILE ORIGINAL_WD
    PACKAGE_NAME="${1}"
    PACKAGE_CONF_DIR="${PACKAGE_CONF_ROOT}/${PACKAGE_NAME}"
    # shellcheck disable=SC2034
    PACKAGE_INSTALL_DIR="${DOTFILES_PACKAGE_INSTALL_DIR}/${PACKAGE_NAME}"
    PACKAGE_INSTALLED_FILE="${DOTFILES_PACKAGE_INSTALL_DIR}/${PACKAGE_NAME}.installed"

    if [ -d "${PACKAGE_CONF_DIR}" ] &&
           ! dotfiles-in-list "${DOTFILES_PACKAGES_LOADED_ENV}" "${PACKAGE_NAME}" &&
           ! dotfiles-package-ignored "${PACKAGE_NAME}"; then
        if [ -e "${PACKAGE_INSTALLED_FILE}" ] || [ ! -e "${PACKAGE_CONF_DIR}/setup.bash" ]; then

            if [ -d "${PACKAGE_CONF_DIR}/bin" ]; then
                export PATH="${PACKAGE_CONF_DIR}/bin:${PATH}"
            fi

            if [ -d "${PACKAGE_CONF_DIR}/python" ]; then
                export PYTHONPATH="${PACKAGE_CONF_DIR}/python:${PYTHONPATH}"
            fi

            dotfiles-symlink-dir-contents "${PACKAGE_CONF_DIR}/nemo-scripts" "${HOME}/.local/share/nemo/scripts" || return 1

            if [ -e "${PACKAGE_CONF_DIR}/env.sh" ]; then
                ORIGINAL_WD="${PWD}"
                cd "${PACKAGE_CONF_DIR}" || return 1
                . "${PACKAGE_CONF_DIR}/env.sh"
                cd "${ORIGINAL_WD}" || return 1
            fi

            export DOTFILES_PACKAGES_LOADED_ENV="${DOTFILES_PACKAGES_LOADED_ENV}:${PACKAGE_NAME}"
        fi
    fi
    return 0
}

loadpackageenvcleanup() {
    unset -f loadpackageenv loadpackageenvcleanup
}
