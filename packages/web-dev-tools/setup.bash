function _can-install() {
    # Depends on antigen for npm
    os linux && linux-variant main && can-sudo && package-installed antigen
}

function _install() {
    sudo apt-get -y install tidy &&
    npm install -g csslint eslint jsonlint js-yaml
}

function _update() {
    _install
}
