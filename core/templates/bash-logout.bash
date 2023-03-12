# shellcheck shell=bash

if [ -n "${DOTFILES_LOGOUT_SCRIPT}" ] && [ -f "${DOTFILES_LOGOUT_SCRIPT}" ]; then
    # shellcheck source=/dev/null
    source "${DOTFILES_LOGOUT_SCRIPT}"
fi
