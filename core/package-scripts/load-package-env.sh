# This must work in plain old sh

symlinkdircontents() {
    # shellcheck disable=SC2039
    local SOURCE_DIR TARGET_DIR TEMPFILE line
    SOURCE_DIR="${1}"
    TARGET_DIR="${2}"

    if [ -d "${SOURCE_DIR}" ] && [ -d "${TARGET_DIR}" ]; then
        TEMPFILE="$(mktemp)" &&
        ls -1 "${SOURCE_DIR}" > "${TEMPFILE}" || return 1
        while read -r line; do
            if [ ! -e "${TARGET_DIR}/${line}" ]; then
                ln -s "${SOURCE_DIR}/${line}" "${TARGET_DIR}/${line}" || return 1
            fi
        done < "${TEMPFILE}"
        rm "${TEMPFILE}" || return 1
    fi
    return 0
}

loadpackageenv() {
    # shellcheck disable=SC2039
    local PACKAGE_NAME PACKAGE_CONF_DIR PACKAGE_INSTALL_DIR PACKAGE_INSTALLED_FILE ORIGINAL_WD
    PACKAGE_NAME="${1}"
    PACKAGE_CONF_DIR="${PACKAGE_CONF_ROOT}/${PACKAGE_NAME}"
    # shellcheck disable=SC2034
    PACKAGE_INSTALL_DIR="${DOTFILES_PACKAGE_INSTALL_DIR}/${PACKAGE_NAME}"
    PACKAGE_INSTALLED_FILE="${DOTFILES_PACKAGE_INSTALL_DIR}/${PACKAGE_NAME}.installed"

    if [ -d "${PACKAGE_CONF_DIR}" ] &&
           ! sh "${DOTFILES_PACKAGE_SCRIPTS}/inlist.sh" "${DOTFILES_PACKAGES_LOADED_ENV}" "${PACKAGE_NAME}" &&
           ! ignored "${PACKAGE_NAME}"; then
        if [ -e "${PACKAGE_INSTALLED_FILE}" ] || [ ! -e "${PACKAGE_CONF_DIR}/setup.bash" ]; then

            if [ -d "${PACKAGE_CONF_DIR}/bin" ]; then
                addpath "${PACKAGE_CONF_DIR}/bin"
            fi

            if [ -d "${PACKAGE_CONF_DIR}/python" ]; then
                export PYTHONPATH="${PACKAGE_CONF_DIR}/python:${PYTHONPATH}"
            fi

            symlinkdircontents "${PACKAGE_CONF_DIR}/nemo-scripts" "${HOME}/.local/share/nemo/scripts" &&
            symlinkdircontents "${PACKAGE_CONF_DIR}/ipython-startup" "${HOME}/.ipython/profile_default/startup" || return 1

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
    unset -f symlinkdircontents loadpackageenv loadpackageenvcleanup
}
