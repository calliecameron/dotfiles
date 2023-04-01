function _can-install() {
    dotfiles-linux-variant main && dotfiles-can-sudo
}

function _install() {
    mkdir -p "${PACKAGE_INSTALL_DIR}" &&
    cd "${PACKAGE_INSTALL_DIR}" &&
    sudo apt-get -y install --install-recommends wine-installer gnome-exe-thumbnailer xserver-xephyr x11vnc xdotool vinagre &&
    wget -O complete.zip 'http://www.ldraw.org/library/updates/complete.zip' &&
    unzip complete.zip &&
    cd ldraw &&
    wget -O mlcad.zip 'http://mlcad.lm-software.com/MLCad_V3.40.zip' &&
    echo '0a95e93dc59c059274255df7c1e30396d30472dfa2649f02f0657923abe282d8  mlcad.zip' > checksum &&
    sha256sum -c checksum &&
    unzip mlcad.zip &&
    chmod u+x MLCad_V3.40/MLCAD.exe &&
    wget -O make-list 'https://raw.githubusercontent.com/nathaneltitane/l2cu/c6881c35fd033b3b3a63d4dfd4f779cfc1caaff5/make-list' &&
    chmod u+x make-list &&
    ./make-list -d &&
    mkdir -p "${PACKAGE_INSTALL_DIR}/bin" &&
    cp "${PACKAGE_CONF_DIR}/mlcad" "${PACKAGE_INSTALL_DIR}/bin/mlcad" &&
    cp "${PACKAGE_CONF_DIR}/mlcad-hidpi" "${PACKAGE_INSTALL_DIR}/bin/mlcad-hidpi" &&
    sed "s|@@@@@1@@@@@|${PACKAGE_CONF_DIR}/logo.png|g" < "${PACKAGE_CONF_DIR}/mlcad-desktop-template" > "${PACKAGE_CONF_DIR}/mlcad.desktop" &&
    chmod u+x "${PACKAGE_CONF_DIR}/mlcad.desktop" &&
    xdg-desktop-menu install --novendor "${PACKAGE_CONF_DIR}/mlcad.desktop" &&
    xdg-desktop-icon install --novendor "${PACKAGE_CONF_DIR}/mlcad.desktop"
}

function _update() {
    cd "${PACKAGE_INSTALL_DIR}" &&
    wget -O complete.zip 'http://www.ldraw.org/library/updates/complete.zip' &&
    unzip -o complete.zip &&
    cd ldraw &&
    ./make-list -d
}
