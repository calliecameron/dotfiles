# shellcheck shell=sh

if env -i which signal-desktop >/dev/null; then
    DOTFILES_ORIGINAL_SIGNAL="$(env -i which signal-desktop)"
    export DOTFILES_ORIGINAL_SIGNAL
fi
