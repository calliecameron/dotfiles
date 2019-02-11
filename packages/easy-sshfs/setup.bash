function _can-install() {
    os linux && linux-variant main pi && can-sudo
}

function _install() {
    sudo apt-get -y install sshfs
}
