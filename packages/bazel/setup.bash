function _can-install() {
    # Depends on antigen for npm
    dotfiles-linux-variant main && package-installed antigen
}

function _install() {
    npm install -g @bazel/bazelisk @bazel/buildifier @bazel/buildozer
}

function _update() {
    _install
}
