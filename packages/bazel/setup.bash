function _can-install() {
    # Depends on web-dev-tools for npm
    os linux && linux-variant main && package-installed web-dev-tools
}

function _install() {
    npm install -g @bazel/bazelisk @bazel/buildifier @bazel/buildozer
}

function _update() {
    _install
}
