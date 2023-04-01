function _can-install() {
    dotfiles-linux-variant main &&
    dotfiles-can-sudo &&
    dotfiles-graphical &&
    package-installed iptables &&
    type port &>/dev/null
}

function _install() {
    "${PACKAGE_CONF_DIR}/setup-dropbox-gui.sh"
}
