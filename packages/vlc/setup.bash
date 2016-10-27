function _can-install() {
    os linux &&
    linux-variant main &&
    graphical &&
    package-installed iptables &&
    type port &>/dev/null &&
    type vlc &>/dev/null
}

function _install() {
    "${PACKAGE_CONF_DIR}/setup-vlc.sh"
}
