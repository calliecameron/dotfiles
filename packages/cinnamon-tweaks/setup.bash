function _can-install() {
    dotfiles-linux-variant main &&
    dotfiles-graphical &&
    pgrep cinnamon &>/dev/null
}

function _install() {
    "${PACKAGE_CONF_DIR}/setup-cinnamon.sh"
}

function _update() {
    _install
}
