function _can-install() {
    [ "${DOTFILES_OS}" = 'linux' ] &&
    [ "${DOTFILES_LINUX_VARIANT}" = 'main' ] &&
    package-installed haskell-stack &&
    type stack &>/dev/null
}

function _install() {
    mkdir "${PACKAGE_INSTALL_DIR}" &&
    cd "${PACKAGE_INSTALL_DIR}" &&
    wget 'https://hackage.haskell.org/package/pandoc-1.17.0.3/pandoc-1.17.0.3.tar.gz' &&
    tar -xf 'pandoc-1.17.0.3.tar.gz' &&
    rm 'pandoc-1.17.0.3.tar.gz' &&
    cd 'pandoc-1.17.0.3' &&
    stack setup &&
    stack install
}
