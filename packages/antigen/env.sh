export ANTIGEN_INSTALL_DIR="${PACKAGE_INSTALL_DIR}"
. "${PACKAGE_INSTALL_DIR}/antigen-env.sh"

if [ -z "${DOTFILES_ANTIGEN_CORE_ONLY}" ]; then
    antigen env callumcameron/argtypes
    antigen env callumcameron/argus
    antigen env callumcameron/distributor
    antigen env callumcameron/markdown-makefile
fi

antigen env callumcameron/emacs-launchers
