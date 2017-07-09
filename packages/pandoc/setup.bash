function _can-install() {
    os linux &&
    linux-variant main &&
    package-installed haskell-stack &&
    type stack &>/dev/null
}

function _install() {
    local TMPDIR
    if is64bit; then
        TMPDIR="$(mktemp -d)" &&
        wget -O "${TMPDIR}/pandoc.deb" 'https://github.com/jgm/pandoc/releases/download/1.19.2.1/pandoc-1.19.2.1-1-amd64.deb' &&
        sudo dpkg -i "${TMPDIR}/pandoc.deb" &&
        rm "${TMPDIR}/pandoc.deb" &&
        rmdir "${TMPDIR}" || return 1
    else
        mkdir "${PACKAGE_INSTALL_DIR}" &&
        cd "${PACKAGE_INSTALL_DIR}" &&
        wget 'https://hackage.haskell.org/package/pandoc-1.19.2.1/pandoc-1.19.2.1.tar.gz' &&
        tar -xf 'pandoc-1.19.2.1.tar.gz' &&
        rm 'pandoc-1.19.2.1.tar.gz' &&
        cd 'pandoc-1.19.2.1' &&
        stack setup &&
        stack install
    fi
}
