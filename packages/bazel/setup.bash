function _can-install() {
    # Depends on antigen for npm
    os linux && linux-variant main && package-installed antigen
}

function _install() {
    source "${NVM_DIR}/nvm.sh" &&
    nvm use node &&
    npm install -g @bazel/bazelisk @bazel/buildifier @bazel/buildozer
}

function _update() {
    _install
}
