function _can-install() {
    [ "${DOTFILES_OS}" = 'linux' ] &&
    ( [ "${DOTFILES_LINUX_VARIANT}" = 'main' ] || [ "${DOTFILES_LINUX_VARIANT}" = 'android' ] ) &&
    [ ! -z "${DOTFILES_CAN_SUDO}" ]
}

function _install() {
    sudo apt-get -y install exuberant-ctags
}

function _update() {
    _install
}
