function _can-install() {
    [ "${DOTFILES_OS}" != 'android' ]
}

function _install() {
    git clone https://github.com/callumcameron/antigen "${PACKAGE_INSTALL_DIR}" &&
    cd "${PACKAGE_INSTALL_DIR}" &&
    git checkout bash-support-temp
}

function _update() {
    zsh -c "source \"${PACKAGE_INSTALL_DIR}/antigen.zsh\" && antigen selfupdate && antigen update"
}
