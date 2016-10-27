function _can-install() {
    (os linux && known-linux-variant && [ -e '/bin/zsh' ]) ||
    os cygwin
}

function _install() {
    if os linux; then
        chsh -s /bin/zsh
    elif os cygwin; then
        echo-blue "The default shell must be set manually on Cygwin; see '${DOTFILES_DIR}/misc/cygwin/README.md' for details."
    fi
}

function _update() {
    _install
}
