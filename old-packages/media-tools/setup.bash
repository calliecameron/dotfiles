function _can-install() {
    dotfiles-linux-variant main && dotfiles-can-sudo
}

function _install() {
    sudo apt-get -y install abcde eyed3 gimp lame handbrake
}
