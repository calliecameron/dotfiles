function _can-install() {
    [ "${DOTFILES_OS}" = 'linux' ] &&
    [ "${DOTFILES_LINUX_VARIANT}" = 'main' ] &&
    [ ! -z "${DOTFILES_CAN_SUDO}" ]
}

function _install() {
    sudo apt-get -y install nodejs-legacy npm &&
    mkdir -p "${HOME}/.npm-packages"
}
