function _can-install() {
    dotfiles-is-graphical && dotfiles-can-sudo
}

function _install() {
    sudo apt-get -y install emacs emacs-el emacs-goodies-el &&
    mkdir "${PACKAGE_INSTALL_DIR}" &&
    "${PACKAGE_SOURCE_DIR}/install-emacs-from-source.sh" "${PACKAGE_INSTALL_DIR}" &&
    sed "s|@@@@@1@@@@@|${PACKAGE_SOURCE_DIR}|g" < "${PACKAGE_SOURCE_DIR}/emacs-daemon.desktop.template" > "${PACKAGE_SOURCE_DIR}/emacs-daemon.desktop" &&
    xdg-desktop-menu install --novendor "${PACKAGE_SOURCE_DIR}/emacs-daemon.desktop" &&
    xdg-desktop-icon install --novendor "${PACKAGE_SOURCE_DIR}/emacs-daemon.desktop" &&
    xdg-mime default emacs-daemon.desktop \
                text/english \
                text/plain \
                text/x-makefile \
                text/x-c++hdr \
                text/x-c++src \
                text/x-chdr \
                text/x-csrc \
                text/x-java \
                text/x-moc \
                text/x-pascal \
                text/x-tcl \
                text/x-tex \
                application/x-shellscript \
                text/x-c \
                text/x-c++ &&
    mkdir -p "${HOME}/.config/autostart" &&
    cp "${PACKAGE_SOURCE_DIR}/emacs-daemon.desktop" "${HOME}/.config/autostart/emacs-daemon.desktop"
}
