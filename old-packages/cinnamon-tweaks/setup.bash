function _can-install() {
    dotfiles-linux-variant main &&
    dotfiles-is-graphical &&
    pgrep cinnamon &>/dev/null
}

function _install() {
    "${PACKAGE_SOURCE_DIR}/setup-cinnamon.sh"
}

function _update() {
    _install
}
