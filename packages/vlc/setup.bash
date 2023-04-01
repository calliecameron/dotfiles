function _can-install() {
    dotfiles-linux-variant main &&
    dotfiles-graphical &&
    package-installed iptables &&
    package-installed emacs &&
    type port &>/dev/null
}

function _install() {
    "${PACKAGE_CONF_DIR}/setup-vlc.sh"
}
