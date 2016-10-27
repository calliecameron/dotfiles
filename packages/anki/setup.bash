function _can-install() {
    os linux && linux-variant main && can-sudo && graphical
}

function _install() {
    # shellcheck disable=SC2155
    local TMPDIR="$(mktemp -d)" &&
    local DEBFILE="${TMPDIR}/anki-2.0.36.deb" &&
    wget -O "${DEBFILE}" 'http://ankisrs.net/download/mirror/anki-2.0.36.deb' &&
    sudo gdebi "${DEBFILE}" &&
    rm "${DEBFILE}" &&
    rmdir "${TMPDIR}"
}
