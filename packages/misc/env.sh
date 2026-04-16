# shellcheck shell=sh

dotfiles-home-link "${PACKAGE_SOURCE_DIR}/dirmngr.conf" "${HOME}/.gnupg/dirmngr.conf"
dotfiles-home-link "${PACKAGE_SOURCE_DIR}/gpg.conf" "${HOME}/.gnupg/gpg.conf"
dotfiles-home-link "${PACKAGE_SOURCE_DIR}/inputrc"
dotfiles-home-link "${PACKAGE_SOURCE_DIR}/ssh_config" "${HOME}/.ssh/config"
dotfiles-home-link "${PACKAGE_SOURCE_DIR}/tmux.conf"
