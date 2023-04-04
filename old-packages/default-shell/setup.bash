function _can-install() {
    dotfiles-known-linux-variant && [ -e '/bin/zsh' ]
}

function _install() {
    chsh -s /bin/zsh
}

function _update() {
    _install
}
