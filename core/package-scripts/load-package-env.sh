# This must work in plain old sh

loadpackageenv() {
    # shellcheck disable=SC2039
    local PACKAGE_NAME PACKAGE_SOURCE_DIR PACKAGE_INSTALL_DIR PACKAGE_INSTALLED_FILE ORIGINAL_WD
    PACKAGE_NAME="${1}"
    PACKAGE_SOURCE_DIR="${PACKAGE_ROOT}/${PACKAGE_NAME}"
    # shellcheck disable=SC2034
    PACKAGE_INSTALL_DIR="${DOTFILES_PACKAGE_INSTALL_DIR}/${PACKAGE_NAME}"
    PACKAGE_INSTALLED_FILE="${DOTFILES_PACKAGE_INSTALL_DIR}/${PACKAGE_NAME}.installed"

    if [ -d "${PACKAGE_SOURCE_DIR}" ] &&
           ! dotfiles-in-list "${DOTFILES_PACKAGES_LOADED_ENV}" "${PACKAGE_NAME}" &&
           ! dotfiles-package-ignored "${PACKAGE_NAME}"; then
        if [ -e "${PACKAGE_INSTALLED_FILE}" ] || [ ! -e "${PACKAGE_SOURCE_DIR}/setup.bash" ]; then

            if [ -d "${PACKAGE_SOURCE_DIR}/bin" ]; then
                export PATH="${PACKAGE_SOURCE_DIR}/bin:${PATH}"
            fi

            dotfiles-symlink-dir-contents "${PACKAGE_SOURCE_DIR}/nemo-scripts" "${HOME}/.local/share/nemo/scripts" || return 1

            if [ -e "${PACKAGE_SOURCE_DIR}/env.sh" ]; then
                ORIGINAL_WD="${PWD}"
                cd "${PACKAGE_SOURCE_DIR}" || return 1
                . "${PACKAGE_SOURCE_DIR}/env.sh"
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
