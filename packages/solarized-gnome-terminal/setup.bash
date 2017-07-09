function _can-install() {
    type gnome-terminal &>/dev/null
}

function _install() {
    git clone https://github.com/Anthony25/gnome-terminal-colors-solarized.git "${PACKAGE_INSTALL_DIR}" || return 1
    if ! "${PACKAGE_INSTALL_DIR}/install.sh" --scheme light --profile Default --skip-dircolors; then
        echo-red "If this fails to install, change the profile name in Gnome Terminal to 'Default'"
        return 1
    fi
    return 0
}

function _update() {
    git pull &&
    "${PACKAGE_INSTALL_DIR}/install.sh" --scheme light --profile Default --skip-dircolors
}
