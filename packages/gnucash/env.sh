if os linux && [ "${DOTFILES_LINUX_VARIANT}" = 'main' ]; then
    export DOTFILES_ORIGINAL_GNUCASH="$(env -i which gnucash)"
fi
