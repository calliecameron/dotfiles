function _can-install() {
    os linux &&
    linux-variant main &&
    graphical &&
    pgrep cinnamon &>/dev/null
}

function _install() {
    "${PACKAGE_CONF_DIR}/setup-cinnamon.sh"
}

function _update() {
    _install
}
