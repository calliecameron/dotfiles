# shellcheck shell=sh

export PATH="${PACKAGE_INSTALL_DIR}/emacs/bin:${PATH}"

export EMACS_LAUNCHERS_DIR="${PACKAGE_INSTALL_DIR}/emacs-launchers"
. "${EMACS_LAUNCHERS_DIR}/env.sh"
