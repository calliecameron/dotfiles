# shellcheck shell=dash
# Must work in dash, bash and zsh

loadpackageenv() {
    local PACKAGE_ROOT PACKAGE_NAME PACKAGE_SOURCE_DIR PACKAGE_INSTALL_DIR ORIGINAL_WD
    PACKAGE_ROOT="${1}"
    PACKAGE_NAME="${2}"
    PACKAGE_SOURCE_DIR="${PACKAGE_ROOT}/${PACKAGE_NAME}"
    # shellcheck disable=SC2034
    PACKAGE_INSTALL_DIR="${DOTFILES_PACKAGE_INSTALL_DIR}/${PACKAGE_NAME}"

    if [ -d "${PACKAGE_SOURCE_DIR}" ] &&
        ! dotfiles-in-list "${DOTFILES_PACKAGES_LOADED_ENV}" "${PACKAGE_NAME}" &&
        ! dotfiles-package-ignored "${PACKAGE_NAME}" &&
        (dotfiles-package-installed "${PACKAGE_NAME}" ||
            ! dotfiles-package-has-installer "${PACKAGE_SOURCE_DIR}"); then

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
    return 0
}

loadpackageenvcleanup() {
    unset -f loadpackageenv loadpackageenvcleanup
}
