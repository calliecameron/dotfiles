# shellcheck shell=bash

if [ -z "${DOTFILES_NO_SOLARIZED_DIRCOLORS}" ] &&
    [ -x /usr/bin/dircolors ]; then
    eval "$(dircolors -b "${PACKAGE_INSTALL_DIR}/solarized/dircolors.ansi-dark")"
fi
