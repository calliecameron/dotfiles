function _can-install() {
    os linux && linux-variant main && can-sudo
}

function _install() {
    "${PACKAGE_CONF_DIR}/setup-iptables.sh"
}
