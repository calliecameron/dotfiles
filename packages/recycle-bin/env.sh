# shellcheck shell=sh

export DOTFILES_RECYCLEBIN="${HOME}/RecycleBin"

if [ ! -e "${DOTFILES_RECYCLEBIN}" ]; then
    mkdir -p "${DOTFILES_RECYCLEBIN}"
fi

if [ ! -d "${DOTFILES_RECYCLEBIN}" ]; then
    dotfiles-log-package-problem "RecycleBin exists but is not a directory."
fi
