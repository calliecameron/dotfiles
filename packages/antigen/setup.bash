function _can-install() {
    ! os android
}

function _install() {
    git clone https://github.com/callumcameron/antigen "${PACKAGE_INSTALL_DIR}" &&
    cd "${PACKAGE_INSTALL_DIR}" &&
    git checkout bash-support-temp
}

function _update() {
    zsh -c "PACKAGE_INSTALL_DIR=\"${PACKAGE_INSTALL_DIR}\"; source \"${PACKAGE_CONF_DIR}/aliases.zsh\" && antigen selfupdate && antigen update"
}
