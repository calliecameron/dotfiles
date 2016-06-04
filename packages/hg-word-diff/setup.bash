function _can-install() {
    type hg &>/dev/null
}

function _install() {
    git clone https://github.com/junghans/cwdiff.git "${PACKAGE_INSTALL_DIR}" &&
    cd "${PACKAGE_INSTALL_DIR}" &&
    git checkout v0.3.0 || return 1

    local LINE="%include ${PACKAGE_INSTALL_DIR}/cwdiff.rc"
    if ! grep "^${LINE}\$" "${DOTFILES_HG_LOCAL}" &>/dev/null; then
        echo "${LINE}" >> "${DOTFILES_HG_LOCAL}" || return 1
    fi
    return 0
}

function _update() {
    local LINE="%include ${PACKAGE_INSTALL_DIR}/cwdiff.rc"
    if ! grep "^${LINE}\$" "${DOTFILES_HG_LOCAL}" &>/dev/null; then
        echo "${LINE}" >> "${DOTFILES_HG_LOCAL}" || return 1
    fi
    return 0
}
