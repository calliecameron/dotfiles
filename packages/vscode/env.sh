# shellcheck shell=sh

dotfiles-home-link "${PACKAGE_SOURCE_DIR}/settings.json" "${HOME}/.config/VSCodium/User/settings.json"
dotfiles-home-link "${PACKAGE_SOURCE_DIR}/keybindings.json" "${HOME}/.config/VSCodium/User/keybindings.json"

if env -i which codium >/dev/null; then
    DOTFILES_ORIGINAL_VSCODE="$(env -i which codium)"
    export DOTFILES_ORIGINAL_VSCODE
fi
