function _can-install() {
    [ "${DOTFILES_OS}" = 'linux' ] &&
    [ "${DOTFILES_LINUX_VARIANT}" = 'main' ] &&
    [ ! -z "${DOTFILES_CAN_SUDO}" ]
}

function _install() {
    local URL DIR
    local TARFILE="${PACKAGE_INSTALL_DIR}/stack.tar.gz"
    if is64bit -q; then
        URL='https://www.stackage.org/stack/linux-x86_64'
        DIR='stack-1.1.2-linux-x86_64'
    else
        URL='https://www.stackage.org/stack/linux-i386'
        DIR='stack-1.1.2-linux-i386'
    fi

    sudo apt-get -y install libgmp-dev &&
    mkdir "${PACKAGE_INSTALL_DIR}" &&
    wget -O "${TARFILE}" "${URL}" &&
    cd "${PACKAGE_INSTALL_DIR}" &&
    tar -xf "${TARFILE}" &&
    mv "${PACKAGE_INSTALL_DIR}/${DIR}" "${PACKAGE_INSTALL_DIR}/stack" &&
    rm "${TARFILE}" &&

    export PATH="${HOME}/.local/bin:${PACKAGE_INSTALL_DIR}/stack:${PATH}" &&
    stack setup &&
    stack install hlint ShellCheck
}
