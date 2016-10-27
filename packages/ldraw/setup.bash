function _can-install() {
    os linux && linux-variant main && can-sudo
}

function _install() {
    sudo apt-get -y install wine wine-gecko2.21 wine-mono0.0.8 gnome-exe-thumbnailer &&
    mkdir -p "${PACKAGE_INSTALL_DIR}" &&
    cd "${PACKAGE_INSTALL_DIR}" &&
    wget -O complete.zip 'http://www.ldraw.org/library/updates/complete.zip' &&
    unzip complete.zip &&
    cd ldraw &&
    wget -O mlcad.zip 'http://mlcad.lm-software.com/MLCad_V3.40.zip' &&
    unzip mlcad.zip &&
    chmod u+x MLCad_V3.40/MLCAD.exe &&
    wget -O make-list 'https://raw.githubusercontent.com/nathaneltitane/ldraw/master/make-list' &&
    chmod u+x make-list &&
    ./make-list -d &&
    mkdir -p "${PACKAGE_INSTALL_DIR}/bin" &&
    cp "${PACKAGE_CONF_DIR}/mlcad" "${PACKAGE_INSTALL_DIR}/bin/mlcad" &&
    sed "s|@@@@@1@@@@@|${PACKAGE_CONF_DIR}/logo.png|g" < "${PACKAGE_CONF_DIR}/mlcad-desktop-template" > "${PACKAGE_CONF_DIR}/mlcad.desktop" &&
    chmod u+x "${PACKAGE_CONF_DIR}/mlcad.desktop" &&
    xdg-desktop-menu install --novendor "${PACKAGE_CONF_DIR}/mlcad.desktop" &&
    xdg-desktop-icon install --novendor "${PACKAGE_CONF_DIR}/mlcad.desktop"
}

function _update() {
    cd "${PACKAGE_INSTALL_DIR}" &&
    wget -O complete.zip 'http://www.ldraw.org/library/updates/complete.zip' &&
    unzip complete.zip &&
    cd ldraw &&
    ./make-list -d
}
