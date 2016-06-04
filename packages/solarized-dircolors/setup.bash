function _can-install() {
    [ "${DOTFILES_OS}" = 'linux' ] || [ "${DOTFILES_OS}" = 'cygwin' ]
}

function _install() {
    git clone https://github.com/seebi/dircolors-solarized.git "${PACKAGE_INSTALL_DIR}"
}

function _update() {
    git pull
}
