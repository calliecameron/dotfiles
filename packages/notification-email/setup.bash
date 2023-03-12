function _can-install() {
    linux-variant main && can-sudo
}

function _install() {
    sudo apt-get -y install ssmtp
}
