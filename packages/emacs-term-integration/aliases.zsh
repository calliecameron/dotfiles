precmd_functions=($precmd_functions emacs-dir-tracking)

if [ "${TERM:0:4}" = "eat-" ] && [ -d "${DOTFILES_EAT_INTEGRATION_DIR}" ]; then
    source "${DOTFILES_EAT_INTEGRATION_DIR}/zsh"
fi

if [ -f "${HOME}/.emacs.d/term-alert/setup.zsh" ]; then
    source "${HOME}/.emacs.d/term-alert/setup.zsh"
else
    source "${PACKAGE_INSTALL_DIR}/term-alert/setup/setup.zsh"
fi
