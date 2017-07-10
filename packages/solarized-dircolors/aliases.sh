if [ -z "${DOTFILES_NO_SOLARIZED_DIRCOLORS}" ]; then
    test -x /usr/bin/dircolors && eval "$(dircolors -b "${PACKAGE_INSTALL_DIR}/dircolors.ansi-light")"
fi
