function _can-install() {
    type gnome-terminal &>/dev/null
}

function _install() {
    git clone https://github.com/Anthony25/gnome-terminal-colors-solarized.git "${PACKAGE_INSTALL_DIR}" &&
    "${PACKAGE_INSTALL_DIR}/install.sh" --scheme dark --profile Default
}

function _update() {
    git pull &&
    "${PACKAGE_INSTALL_DIR}/install.sh" --scheme dark --profile Default
}
