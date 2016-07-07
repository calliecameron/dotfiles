function _can-install() {
    [ "${DOTFILES_OS}" = 'linux' ] &&
    [ "${DOTFILES_LINUX_VARIANT}" = 'main' ] &&
    [ ! -z "${DOTFILES_CAN_SUDO}" ] &&
    package-installed iptables &&
    type port &>/dev/null
}

function _install() {
    # Don't run at startup
    if lsb_release -a 2>/dev/null | grep rosa &>/dev/null; then
        echo 'manual' | sudo tee /etc/init/smbd.override &>/dev/null &&
        echo 'manual' | sudo tee /etc/init/nmbd.override &>/dev/null || return 1
    else
	if sudo systemctl status smbd &>/dev/null; then
	    sudo systemctl stop smbd nmbd &&
            sudo systemctl disable smbd nmbd || return 1
        fi
    fi
    return 0
}

function _update() {
    _install
}
