function _can-install() {
    linux-variant main &&
    graphical &&
    package-installed iptables &&
    package-installed emacs &&
    type port &>/dev/null
}

function _install() {
    "${PACKAGE_CONF_DIR}/setup-vlc.sh"
}
