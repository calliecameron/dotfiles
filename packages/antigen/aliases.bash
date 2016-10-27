source "${PACKAGE_INSTALL_DIR}/antigen.bash"

if [ -z "${DOTFILES_ANTIGEN_CORE_ONLY}" ]; then
    antigen bundle callumcameron/argtypes
    antigen bundle callumcameron/argus
    antigen bundle callumcameron/distributor
    antigen bundle callumcameron/markdown-makefile
fi

antigen bundle callumcameron/emacs-launchers
