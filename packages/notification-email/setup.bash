function _can-install() {
    os linux &&
    which ssmtp &>/dev/null
}

function _install() {
    true
}
