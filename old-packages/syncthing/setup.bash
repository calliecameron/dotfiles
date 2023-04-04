function _can-install() {
    dotfiles-linux-variant main &&
    dotfiles-can-sudo &&
    dotfiles-package-installed iptables &&
    type port &>/dev/null
}

function _install() {
    "${PACKAGE_SOURCE_DIR}/setup-syncthing.sh"
}
