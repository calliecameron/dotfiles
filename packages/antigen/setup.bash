function _install() {
    git clone https://github.com/zsh-users/antigen "${PACKAGE_INSTALL_DIR}"
}

function _update() {
    zsh -c "PACKAGE_INSTALL_DIR=\"${PACKAGE_INSTALL_DIR}\"; source \"${PACKAGE_CONF_DIR}/aliases.zsh\" && antigen selfupdate && antigen update"
}
