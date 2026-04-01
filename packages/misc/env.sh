# shellcheck shell=sh

dotfiles-home-link "${PACKAGE_SOURCE_DIR}/inputrc"
dotfiles-home-link "${PACKAGE_SOURCE_DIR}/ssh_config" "${HOME}/.ssh/config"
dotfiles-home-link "${PACKAGE_SOURCE_DIR}/tmux.conf"
