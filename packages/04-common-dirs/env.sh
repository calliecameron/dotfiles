# shellcheck shell=sh

dotfiles-home-link "${WINROOT}" "${HOME}/WinRoot"
dotfiles-home-link "${WINHOME}" "${HOME}/WinHome"
dotfiles-home-link "${WINDOWNLOADS}" "${HOME}/WinDownloads"
dotfiles-home-link "${CYGHOME}" "${HOME}/CygHome"

if [ -z "${DROPBOX}" ] && [ -d "${HOME}/Dropbox" ] && [ ! -h "${HOME}/Dropbox" ]; then
    export DROPBOX="${HOME}/Dropbox"
else
    dotfiles-home-link "${DROPBOX}" "${HOME}/Dropbox"
fi
