# shellcheck shell=sh

test -n "${WINROOT:-}" && dotfiles-home-link "${WINROOT}" "${HOME}/WinRoot"
test -n "${WINHOME:-}" && dotfiles-home-link "${WINHOME}" "${HOME}/WinHome"
test -n "${WINDOWNLOADS:-}" && dotfiles-home-link "${WINDOWNLOADS}" "${HOME}/WinDownloads"
test -n "${CYGHOME:-}" && dotfiles-home-link "${CYGHOME}" "${HOME}/CygHome"

if [ -z "${DROPBOX}" ] && [ -d "${HOME}/Dropbox" ] && [ ! -h "${HOME}/Dropbox" ]; then
    export DROPBOX="${HOME}/Dropbox"
else
    dotfiles-home-link "${DROPBOX}" "${HOME}/Dropbox"
fi
