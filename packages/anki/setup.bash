function _can-install() {
    [ "${DOTFILES_OS}" = 'linux' ] &&
    [ "${DOTFILES_LINUX_VARIANT}" = 'main' ] &&
    [ ! -z "${DOTFILES_CAN_SUDO}" ] &&
    [ ! -z "${DISPLAY}" ]
}

function _install() {
    # shellcheck disable=SC2155
    local TMPDIR="$(mktemp -d)" &&
    local DEBFILE="${TMPDIR}/anki-2.0.33.deb" &&
    wget -O "${DEBFILE}" 'http://ankisrs.net/download/mirror/anki-2.0.33.deb' &&
    sudo gdebi "${DEBFILE}" &&
    rm "${DEBFILE}" &&
    rmdir "${TMPDIR}"
}
