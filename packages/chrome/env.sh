# shellcheck shell=sh

if env -i which google-chrome-stable >/dev/null; then
    DOTFILES_ORIGINAL_CHROME="$(env -i which google-chrome-stable)"
    export DOTFILES_ORIGINAL_CHROME
fi
