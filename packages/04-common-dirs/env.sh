# shellcheck shell=sh

if [ -z "${DROPBOX}" ] && [ -d "${HOME}/Dropbox" ] && [ ! -h "${HOME}/Dropbox" ]; then
    export DROPBOX="${HOME}/Dropbox"
else
    dotfiles-home-link "${DROPBOX}" "${HOME}/Dropbox"
fi
