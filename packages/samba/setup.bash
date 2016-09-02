function _can-install() {
    [ "${DOTFILES_OS}" = 'linux' ] &&
    [ "${DOTFILES_LINUX_VARIANT}" = 'main' ] &&
    [ ! -z "${DOTFILES_CAN_SUDO}" ] &&
    package-installed iptables &&
    type port &>/dev/null
}

function _install() {
    # Don't run at startup
    if sudo systemctl status smbd &>/dev/null; then
	    sudo systemctl stop smbd nmbd &&
        sudo systemctl disable smbd nmbd || return 1
    fi
    return 0
}

function _update() {
    _install
}
