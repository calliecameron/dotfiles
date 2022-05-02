if [ -n "${DOTFILES_LOGOUT_SCRIPT}" ] && [ -f "${DOTFILES_LOGOUT_SCRIPT}" ]; then
    source "${DOTFILES_LOGOUT_SCRIPT}"
fi
