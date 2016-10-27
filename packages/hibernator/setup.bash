function _can-install() {
    os linux && linux-variant main
}

function _install() {
    "${PACKAGE_CONF_DIR}/setup-hibernator.sh"
}
