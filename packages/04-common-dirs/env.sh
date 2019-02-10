homelink "${WINROOT}" "${HOME}/WinRoot"
homelink "${WINHOME}" "${HOME}/WinHome"
homelink "${WINDOWNLOADS}" "${HOME}/WinDownloads"
homelink "${CYGHOME}" "${HOME}/CygHome"

if [ -z "${DROPBOX}" ] && [ -d "${HOME}/Dropbox" ] && [ ! -h "${HOME}/Dropbox" ]; then
    export DROPBOX="${HOME}/Dropbox"
else
    homelink "${DROPBOX}" "${HOME}/Dropbox"
fi
