function _can-install() {
    ([ "${DOTFILES_OS}" = 'linux' ] &&
    ( [ "${DOTFILES_LINUX_VARIANT}" = 'main' ] || [ "${DOTFILES_LINUX_VARIANT}" = 'android' ] ) &&
    [ ! -z "${DOTFILES_CAN_SUDO}" ] &&
    [ ! -z "${DISPLAY}" ]) ||
    [ "${DOTFILES_OS}" = 'cygwin' ]
}

function _install() {
    if [ "${DOTFILES_OS}" = 'linux' ]; then
        mkdir "${PACKAGE_INSTALL_DIR}" &&
        "${PACKAGE_CONF_DIR}/install-emacs-from-source.sh" "${PACKAGE_INSTALL_DIR}" || return 1
        if [ "${DOTFILES_LINUX_VARIANT}" = 'main' ]; then
            xdg-desktop-menu install --novendor "${PACKAGE_CONF_DIR}/emacs-daemon.desktop" &&
            xdg-desktop-icon install --novendor "${PACKAGE_CONF_DIR}/emacs-daemon.desktop" &&
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
            cp "${PACKAGE_CONF_DIR}/emacs-daemon.desktop" "${HOME}/.config/autostart/emacs-daemon.desktop" || return 1
        fi
        return 0
    elif [ "${DOTFILES_OS}" = 'cygwin' ]; then
        apt-cyg install emacs-w32 libpng-devel zlib-devel libpoppler-glib-devel || return 1
    else
        echo-red "Unknown OS"
        return 1
    fi
}
