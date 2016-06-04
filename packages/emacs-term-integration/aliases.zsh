precmd_functions=($precmd_functions emacs-dir-tracking)

if [ -f "${HOME}/.emacs.d/term-alert/setup.zsh" ]; then
    source "${HOME}/.emacs.d/term-alert/setup.zsh"
else
    source "${PACKAGE_INSTALL_DIR}/term-alert/setup/setup.zsh"
fi
