function _can-install() {
    type qmake &>/dev/null &&
    type cmake &>/dev/null
}

function _install() {
    git clone https://github.com/htacg/tidy-html5.git "${PACKAGE_INSTALL_DIR}" &&

    cd "${PACKAGE_INSTALL_DIR}/build/cmake" &&
    cmake ../.. "-DCMAKE_INSTALL_PREFIX=${PACKAGE_INSTALL_DIR}/installation-dir" &&
    make &&
    make install
}

function _update() {
    git pull &&
    cd build/cmake &&
    cmake ../.. "-DCMAKE_INSTALL_PREFIX=${PACKAGE_INSTALL_DIR}/installation-dir" &&
    make &&
    make install
}
