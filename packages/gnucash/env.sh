if env -i which gnucash >/dev/null; then
    export DOTFILES_ORIGINAL_GNUCASH="$(env -i which gnucash)"
fi
