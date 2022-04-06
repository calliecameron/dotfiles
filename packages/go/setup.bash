function _install() {
    os linux && linux-variant main && package-installed dev-tools
}

function _install() {
    mkdir -p "${PACKAGE_INSTALL_DIR}" &&
    cd "${PACKAGE_INSTALL_DIR}" &&
    git clone https://github.com/moovweb/gvm &&
    GVM_NO_UPDATE_PROFILE=t "${PACKAGE_INSTALL_DIR}/gvm/binscripts/gvm-installer" &&
    zsh -c "PACKAGE_INSTALL_DIR=\"${PACKAGE_INSTALL_DIR}\"; source \"${PACKAGE_CONF_DIR}/env.sh\" && source \"${PACKAGE_CONF_DIR}/aliases.sh\"" &&
    source "${PACKAGE_CONF_DIR}/env.sh" &&
    go install mvdan.cc/sh/v3/cmd/shfmt@latest
}