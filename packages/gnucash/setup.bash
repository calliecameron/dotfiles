function _can-install() {
  os linux && linux-variant main && graphical && can-sudo
}

function _install() {
    mkdir "${PACKAGE_INSTALL_DIR}" &&
    "${PACKAGE_CONF_DIR}/setup-gnucash.sh" "${PACKAGE_INSTALL_DIR}" &&
    xdg-desktop-menu install --novendor "${PACKAGE_INSTALL_DIR}/share/applications/gnucash.desktop"
}
