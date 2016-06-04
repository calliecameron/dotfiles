function _can-install() {
    [ "${DOTFILES_OS}" = 'linux' ] &&
    [ "${DOTFILES_LINUX_VARIANT}" = 'main' ] &&
    [ ! -z "${DOTFILES_CAN_SUDO}" ] &&
    package-installed iptables &&
    type port &>/dev/null
}

function _install() {
    # Don't run at startup
    echo 'manual' | sudo tee /etc/init/smbd.override &>/dev/null &&
    echo 'manual' | sudo tee /etc/init/nmbd.override &>/dev/null
}

function _update() {
    _install
}
