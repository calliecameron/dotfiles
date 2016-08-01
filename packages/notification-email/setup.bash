function _can-install() {
    [ "${DOTFILES_OS}" = 'linux' ] &&
    which ssmtp &>/dev/null
}

function _install() {
    true
}
