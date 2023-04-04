export DOTFILES_RECYCLEBIN="${HOME}/RecycleBin"

# Make sure the recycle bin exists
if [ ! -e "${DOTFILES_RECYCLEBIN}" ]; then
    mkdir -p "${DOTFILES_RECYCLEBIN}"
fi

if [ ! -d "${DOTFILES_RECYCLEBIN}" ]; then
    echo "Warning - RecycleBin exists but is not a directory." >&2
fi
