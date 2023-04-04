function _can-install() {
    dotfiles-package-installed emacs &&
    type emacs &>/dev/null
}

function _install() {
    git clone https://github.com/cask/cask.git "${PACKAGE_INSTALL_DIR}" &&
    cd "${PACKAGE_INSTALL_DIR}" &&
    git checkout v0.8.7
}

# function _update() {
#     # Apparently this doesn't work between stable releases...
#     export PATH="${PACKAGE_INSTALL_DIR}/bin:${PATH}" &&
#     cask upgrade-cask
# }
