source "${PACKAGE_INSTALL_DIR}/antigen.bash"

if [ -z "${DOTFILES_ANTIGEN_CORE_ONLY}" ]; then
    antigen bundle calliecameron/argtypes
    antigen bundle calliecameron/argus
    antigen bundle calliecameron/distributor
    antigen bundle calliecameron/markdown-makefile
fi

antigen bundle calliecameron/emacs-launchers
