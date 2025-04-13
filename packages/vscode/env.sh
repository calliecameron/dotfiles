# shellcheck shell=sh

dotfiles-home-link "${PACKAGE_SOURCE_DIR}/settings.json" "${HOME}/.config/VSCodium/User/settings.json"
dotfiles-home-link "${PACKAGE_SOURCE_DIR}/keybindings.json" "${HOME}/.config/VSCodium/User/keybindings.json"
