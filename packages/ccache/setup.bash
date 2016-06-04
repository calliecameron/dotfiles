function _can-install() {
    ([ "${DOTFILES_OS}" = 'linux' ] &&
    ( [ "${DOTFILES_LINUX_VARIANT}" = 'main' ] || [ "${DOTFILES_LINUX_VARIANT}" = 'android' ] ) &&
    [ ! -z "${DOTFILES_CAN_SUDO}" ]) ||
    [ "${DOTFILES_OS}" = 'cygwin' ]
}

function _install() {
    if [ "${DOTFILES_OS}" = 'linux' ]; then
        sudo apt-get -y install ccache || return 1
    elif [ "${DOTFILES_OS}" = 'cygwin' ]; then
        apt-cyg install ccache || return 1
    else
        echo-red "Unknown OS"
        return 1
    fi
}

function _update() {
    _install
}
