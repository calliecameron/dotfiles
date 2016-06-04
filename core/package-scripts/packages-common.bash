source "${DOTFILES_BASH_COMMON}" || exit 1

IGNORE_FILE="${DOTFILES_PACKAGE_INSTALL_DIR}/ignored.txt"
# shellcheck disable=SC2034
UPDATE_FILE="${DOTFILES_PACKAGE_INSTALL_DIR}/last-update.txt"

function package-setup-vars() {
    # Arg: the name of the package
    PACKAGE_NAME="${1}"
    PACKAGE_CONF_DIR="${PACKAGE_CONF_ROOT}/${PACKAGE_NAME}"
    # shellcheck disable=SC2034
    PACKAGE_INSTALL_DIR="${DOTFILES_PACKAGE_INSTALL_DIR}/${PACKAGE_NAME}"
    # shellcheck disable=SC2034
    PACKAGE_INSTALLED_FILE="${DOTFILES_PACKAGE_INSTALL_DIR}/${PACKAGE_NAME}.installed"
    # shellcheck disable=SC2034
    PACKAGE_SETUP_FILE="${PACKAGE_CONF_DIR}/setup.bash"
}

function package-cleanup() {
    unset -f _install _update _can-install
    unset OFFER_GIT_SSH USE_GIT_SSH
}

function package-installed() {
    local NAME
    if [ ! -z "${1}" ]; then
        NAME="${1}"
    else
        NAME="${PACKAGE_NAME}"
    fi
    test -e "${DOTFILES_PACKAGE_INSTALL_DIR}/${NAME}.installed"
}

function package-ignored() {
    local NAME
    if [ ! -z "${1}" ]; then
        NAME="${1}"
    else
        NAME="${PACKAGE_NAME}"
    fi

    if [ -e "${IGNORE_FILE}" ]; then
        if grep "${NAME}" "${IGNORE_FILE}" &>/dev/null; then
            return 0
        else
            return 1
        fi
    else
        return 1
    fi
}
