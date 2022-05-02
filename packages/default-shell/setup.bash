function _can-install() {
    os linux && known-linux-variant && [ -e '/bin/zsh' ]
}

function _install() {
    chsh -s /bin/zsh
}

function _update() {
    _install
}
