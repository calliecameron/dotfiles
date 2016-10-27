function _can-install() {
    ! os android &&
    ! linux-variant pi &&
    type git &>/dev/null
}

function _install() {
    mkdir -p "${PACKAGE_INSTALL_DIR}" &&
    git clone 'https://github.com/FortAwesome/Font-Awesome.git' "${PACKAGE_INSTALL_DIR}/font-awesome" &&
    git clone 'https://github.com/konpa/devicon.git' "${PACKAGE_INSTALL_DIR}/devicon" &&
    git clone 'https://github.com/fizzed/font-mfizz.git' "${PACKAGE_INSTALL_DIR}/font-mfizz" &&
    git clone 'https://github.com/primer/octicons.git' "${PACKAGE_INSTALL_DIR}/octicons" &&
    git clone 'https://github.com/Keyamoon/IcoMoon-Free.git' "${PACKAGE_INSTALL_DIR}/icomoon" &&

    homelink "${PACKAGE_INSTALL_DIR}/font-awesome/fonts/fontawesome-webfont.ttf" "${HOME}/.fonts/web-fonts/fontawesome-webfont.ttf" &&
    homelink "${PACKAGE_INSTALL_DIR}/devicon/fonts/devicon.ttf" "${HOME}/.fonts/web-fonts/devicon.ttf" &&
    homelink "${PACKAGE_INSTALL_DIR}/font-mfizz/dist/font-mfizz.ttf" "${HOME}/.fonts/web-fonts/font-mfizz.ttf" &&
    homelink "${PACKAGE_INSTALL_DIR}/octicons/build/font/octicons.ttf" "${HOME}/.fonts/web-fonts/octicons.ttf" &&
    homelink "${PACKAGE_INSTALL_DIR}/icomoon/Font/IcoMoon-Free.ttf" "${HOME}/.fonts/web-fonts/IcoMoon-Free.ttf" &&

    fc-cache
}

function _update() {
    cd "${PACKAGE_INSTALL_DIR}/font-awesome" &&
    git pull &&
    cd "${PACKAGE_INSTALL_DIR}/devicon" &&
    git pull &&
    cd "${PACKAGE_INSTALL_DIR}/font-mfizz" &&
    git pull &&
    cd "${PACKAGE_INSTALL_DIR}/octicons" &&
    git pull &&
    cd "${PACKAGE_INSTALL_DIR}/icomoon" &&
    git pull &&
    fc-cache
}
