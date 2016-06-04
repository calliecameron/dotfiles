homelink "${WINROOT}" "${HOME}/WinRoot"
homelink "${WINHOME}" "${HOME}/WinHome"
homelink "${WINDOWNLOADS}" "${HOME}/WinDownloads"
homelink "${CYGHOME}" "${HOME}/CygHome"

if [ "${DOTFILES_OS}" = 'linux' ] && [ "${DOTFILES_LINUX_VARIANT}" = 'android' ]; then
    complainunset 'ANDROID_SDCARD' 'Android sdcard location'
    complainunset 'ANDROID_DOCUMENTS' 'Android documents location'
    complainunset 'ANDROID_DOWNLOADS' 'Android downloads location'
    complainunset 'ANDROID_EXTRA' 'Android extra storage location'
    homelink "${ANDROID_SDCARD}" "${HOME}/sdcard"
    homelink "${ANDROID_DOCUMENTS}" "${HOME}/Documents"
    homelink "${ANDROID_DOWNLOADS}" "${HOME}/Downloads"
    homelink "${ANDROID_EXTRA}" "${HOME}/LinuxExtra"
fi

if [ -z "${DROPBOX}" ] && [ -d "${HOME}/Dropbox" ] && [ ! -h "${HOME}/Dropbox" ]; then
    export DROPBOX="${HOME}/Dropbox"
else
    homelink "${DROPBOX}" "${HOME}/Dropbox"
fi
