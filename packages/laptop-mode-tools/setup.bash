function _can-install() {
    [ "${DOTFILES_OS}" = 'linux' ] &&
    [ "${DOTFILES_LINUX_VARIANT}" = 'main' ] &&
    [ ! -z "${DOTFILES_CAN_SUDO}" ] &&
    type laptop-detect &>/dev/null &&
    laptop-detect
}

function _install() {
    sudo apt-get install laptop-mode-tools
}
