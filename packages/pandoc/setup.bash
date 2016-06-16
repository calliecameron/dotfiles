function _can-install() {
    [ "${DOTFILES_OS}" = 'linux' ] &&
    [ "${DOTFILES_LINUX_VARIANT}" = 'main' ] &&
    package-installed haskell-stack &&
    type stack &>/dev/null
}

function _install() {
    local TMPDIR
    if is64bit -q; then
        TMPDIR="$(mktemp -d)" &&
        wget -O "${TMPDIR}/pandoc.deb" 'https://github.com/jgm/pandoc/releases/download/1.17.1/pandoc-1.17.1-2-amd64.deb' &&
        sudo dpkg -i "${TMPDIR}/pandoc.deb" &&
        rm "${TMPDIR}/pandoc.deb" &&
        rmdir "${TMPDIR}" || return 1
    else
        mkdir "${PACKAGE_INSTALL_DIR}" &&
        cd "${PACKAGE_INSTALL_DIR}" &&
        wget 'https://hackage.haskell.org/package/pandoc-1.17.0.3/pandoc-1.17.0.3.tar.gz' &&
        tar -xf 'pandoc-1.17.0.3.tar.gz' &&
        rm 'pandoc-1.17.0.3.tar.gz' &&
        cd 'pandoc-1.17.0.3' &&
        stack setup &&
        stack install
    fi
}
