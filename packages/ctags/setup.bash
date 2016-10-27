function _can-install() {
    os linux && linux-variant main android && can-sudo
}

function _install() {
    sudo apt-get -y install exuberant-ctags
}

function _update() {
    _install
}
