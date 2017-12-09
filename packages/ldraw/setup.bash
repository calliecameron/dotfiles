function _can-install() {
    os linux && linux-variant main && can-sudo
}

function _install() {
    mkdir -p "${PACKAGE_INSTALL_DIR}" &&
    cd "${PACKAGE_INSTALL_DIR}" &&
    sudo dpkg --add-architecture i386 &&
    wget -nc https://dl.winehq.org/wine-builds/Release.key &&
    sudo apt-key add Release.key &&
    sudo apt-add-repository 'deb https://dl.winehq.org/wine-builds/ubuntu/ xenial main' &&
    sudo apt-get update &&
    sudo apt-get -y install --install-recommends winehq-devel gnome-exe-thumbnailer xserver-xephyr x11vnc xdotool vinagre &&
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
    cp "${PACKAGE_CONF_DIR}/mlcad-hidpi" "${PACKAGE_INSTALL_DIR}/bin/mlcad-hidpi" &&
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
