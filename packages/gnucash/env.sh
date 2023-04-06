# shellcheck shell=sh

if env -i which gnucash >/dev/null; then
    DOTFILES_ORIGINAL_GNUCASH="$(env -i which gnucash)"
    export DOTFILES_ORIGINAL_GNUCASH
fi
