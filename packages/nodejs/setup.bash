function _can-install() {
    os linux && linux-variant main && can-sudo
}

function _install() {
    sudo apt-get -y install nodejs-legacy npm &&
    mkdir -p "${HOME}/.npm-packages"
}
