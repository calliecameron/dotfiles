function _can-install() {
    ([ "${DOTFILES_OS}" = 'linux' ] &&
    [ ! -z "${DOTFILES_LINUX_VARIANT}" ] &&
    [ -e '/bin/zsh' ]) ||
    [ "${DOTFILES_OS}" = 'cygwin' ]
}

function _install() {
    if [ "${DOTFILES_OS}" = 'linux' ]; then
        chsh -s /bin/zsh
    elif [ "${DOTFILES_OS}" = 'cygwin' ]; then
        echo-blue "The default shell must be set manually on Cygwin; see '${DOTFILES_DIR}/misc/cygwin/README.md' for details."
    fi
}

function _update() {
    _install
}
