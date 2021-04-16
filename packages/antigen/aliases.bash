source "${PACKAGE_INSTALL_DIR}/antigen.bash"

if [ -z "${DOTFILES_ANTIGEN_CORE_ONLY}" ]; then
    antigen bundle calliecameron/markdown-makefile
fi

antigen bundle calliecameron/emacs-launchers
