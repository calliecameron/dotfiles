function _can-install() {
    linux-variant main &&
    can-sudo &&
    package-installed iptables &&
    type port &>/dev/null
}

function _install() {
    "${PACKAGE_CONF_DIR}/setup-syncthing.sh"
}
