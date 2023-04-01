# Functions available to setup.bash scripts, and others used by the installation and update scripts.

# shellcheck disable=SC2034
UPDATE_FILE="${DOTFILES_PACKAGE_INSTALL_DIR}/last-update.txt"
# shellcheck disable=SC2034
UPDATED_ALREADY_FILE="${DOTFILES_PACKAGE_INSTALL_DIR}/updated-already.txt"

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
}
