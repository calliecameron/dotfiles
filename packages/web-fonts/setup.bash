function _can-install() {
    [ "${DOTFILES_OS}" != 'android' ] &&
    [ "${DOTFILES_LINUX_VARIANT}" != 'pi' ] &&
    type git &>/dev/null
}

function _install() {
    mkdir -p "${PACKAGE_INSTALL_DIR}" &&
    git clone 'https://github.com/FortAwesome/Font-Awesome.git' "${PACKAGE_INSTALL_DIR}/font-awesome" &&
    git clone 'https://github.com/konpa/devicon.git' "${PACKAGE_INSTALL_DIR}/devicon" &&
    git clone 'https://github.com/fizzed/font-mfizz.git' "${PACKAGE_INSTALL_DIR}/font-mfizz"
}

function _update() {
    cd "${PACKAGE_INSTALL_DIR}/font-awesome" &&
    git pull &&
    cd "${PACKAGE_INSTALL_DIR}/devicon" &&
    git pull &&
    cd "${PACKAGE_INSTALL_DIR}/font-mfizz" &&
    git pull
}
