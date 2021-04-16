export ANTIGEN_INSTALL_DIR="${PACKAGE_INSTALL_DIR}"
. "${PACKAGE_INSTALL_DIR}/antigen-env.sh"

if [ -z "${DOTFILES_ANTIGEN_CORE_ONLY}" ]; then
    antigen env calliecameron/markdown-makefile
fi

antigen env calliecameron/emacs-launchers
