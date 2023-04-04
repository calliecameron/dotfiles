function _can-install() {
    # Depends on antigen for npm
    dotfiles-linux-variant main && dotfiles-can-sudo && dotfiles-package-installed antigen
}

function _install() {
    sudo apt-get -y install tidy &&
    npm install -g csslint eslint jsonlint js-yaml &&

    mkdir -p "${PACKAGE_INSTALL_DIR}" &&
    cd "${PACKAGE_INSTALL_DIR}" &&
    wget https://github.com/mozilla/geckodriver/releases/download/v0.30.0/geckodriver-v0.30.0-linux64.tar.gz &&
    echo '12c37f41d11ed982b7be43d02411ff2c75fb7a484e46966d000b47d1665baa88  geckodriver-v0.30.0-linux64.tar.gz' > checksum &&
    sha256sum -c checksum &&
    tar -xf geckodriver-v0.30.0-linux64.tar.gz &&
    mkdir -p "${PACKAGE_INSTALL_DIR}/bin" &&
    mv "${PACKAGE_INSTALL_DIR}/geckodriver" "${PACKAGE_INSTALL_DIR}/bin"
}

function _update() {
    _install
}
