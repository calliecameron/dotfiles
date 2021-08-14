function _can-install() {
    os linux && linux-variant main && can-sudo
}

function _install() {
    sudo apt-get -y install abcde eyed3 gimp lame handbrake
}
