function _can-install() {
    type gnome-terminal &>/dev/null
}

function _install() {
    if [ -z "${DOTFILES_GNOME_TERMINAL_PROFILE}" ]; then
        echo-red "Set DOTFILES_GNOME_TERMINAL_PROFILE to the profile ID reported by Gnome Terminal to install."
        return 1
    fi
    git clone https://github.com/Anthony25/gnome-terminal-colors-solarized.git "${PACKAGE_INSTALL_DIR}" &&
    "${PACKAGE_INSTALL_DIR}/install.sh" --scheme light --profile "${DOTFILES_GNOME_TERMINAL_PROFILE}" --skip-dircolors
}

function _update() {
    if [ -z "${DOTFILES_GNOME_TERMINAL_PROFILE}" ]; then
        echo-red "Set DOTFILES_GNOME_TERMINAL_PROFILE to the profile ID reported by Gnome Terminal to update."
        return 1
    fi
    git pull &&
    "${PACKAGE_INSTALL_DIR}/install.sh" --scheme light --profile "${DOTFILES_GNOME_TERMINAL_PROFILE}" --skip-dircolors
}
