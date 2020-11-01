function _can-install() {
    os linux && linux-variant main && can-sudo
}

function _install() {
    sudo apt-get -y install tidy nodejs npm &&
    mkdir -p "${HOME}/.npm-packages"
    if [ -d "${HOME}/tmp" ]; then
        rmdir "${HOME}/tmp" || return 1
    fi
    source "${PACKAGE_CONF_DIR}/env.sh" &&
    npm install -g csslint eslint jscs jsonlint js-yaml
}

# function _update() {
#     _install
# }
