#!/bin/bash

sudo apt-get -y install vlc &&

source "${DOTFILES_BASH_COMMON}" &&
source "${THIS_DIR}/env.sh" &&

VLCRC="${HOME}/.config/vlc/vlcrc" &&
mkdir -p "${HOME}/.config/vlc" || exit 1

function check() {
    local SRC="${1}"
    local DST="${2}"

    if ! grep "^${DST}\$" "${VLCRC}" &>/dev/null; then
        if grep "^${SRC}\$" "${VLCRC}" &>/dev/null; then
            sed -i "s|${SRC}|${DST}|g" "${VLCRC}" || return 1
        else
            echo "${DST}" >> "${VLCRC}" || return 1
        fi
    fi
    return 0
}

if [ ! -e "${VLCRC}" ]; then
    echo 'one-instance=1' >> "${VLCRC}" &&
    echo 'playlist-enqueue=1' >> "${VLCRC}" || exit 1
else
    check 'one-instance=0' 'one-instance=1' &&
    check 'playlist-enqueue=0' 'playlist-enqueue=1' || exit 1
fi

if dotfiles-yn-n "Open firewall port for VLC remote control (TCP port ${DOTFILES_VLC_PORT})?"; then
    port open-at-boot "${DOTFILES_VLC_PORT}" tcp &&
    port open "${DOTFILES_VLC_PORT}" tcp || exit 1
else
    port dont-open-at-boot "${DOTFILES_VLC_PORT}" tcp || exit 1
fi


xdg-desktop-menu install --novendor "${THIS_DIR}/vlc.desktop" &&
xdg-desktop-icon install --novendor "${THIS_DIR}/vlc.desktop" &&

xdg-mime default vlc.desktop application/vnd.adobe.flash.movie application/xspf+xml audio/mpeg audio/mp4 audio/x-mpegurl audio/x-vorbis+ogg audio/x-wav video/mpeg video/mp4 video/ogg video/quicktime video/x-msvideo video/x-matroska &&

"${THIS_DIR}/non-default-association.el" 'inode/directory' 'vlc.desktop' "${HOME}/.config/mimeapps.list"
