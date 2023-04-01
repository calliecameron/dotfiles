function _can-install() {
    dotfiles-linux-variant main &&
    dotfiles-can-sudo &&
    type laptop-detect &>/dev/null &&
    laptop-detect
}

function _install() {
    sudo apt-get -y install laptop-mode-tools
}
