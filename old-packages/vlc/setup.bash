function _can-install() {
    dotfiles-linux-variant main &&
    dotfiles-is-graphical &&
    dotfiles-package-installed iptables &&
    dotfiles-package-installed emacs &&
    type port &>/dev/null
}

function _install() {
    "${PACKAGE_SOURCE_DIR}/setup-vlc.sh"
}
