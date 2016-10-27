function _can-install() {
    os linux &&
    linux-variant main &&
    can-sudo &&
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
