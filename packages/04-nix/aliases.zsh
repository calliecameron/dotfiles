if [ -e "${DOTFILES_HOME_MANAGER_ZSHENV}" ]; then
    source "${DOTFILES_HOME_MANAGER_ZSHENV}"
fi

if [ -e "${DOTFILES_HOME_MANAGER_ZSHRC}" ]; then
    source "${DOTFILES_HOME_MANAGER_ZSHRC}"
fi

fpath=("${HOME}/.nix-profile/share/zsh/site-functions" $fpath)
