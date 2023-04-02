function _can-install() {
    dotfiles-known-linux-variant && dotfiles-can-sudo
}

function _install() {
    chmod go-rwx ~ || return 1

    sudo apt-get update &&
    sudo apt-get upgrade &&
    sudo apt-get dist-upgrade || return 1

    if dotfiles-linux-variant main; then
        "${PACKAGE_SOURCE_DIR}/packages-main.sh" || return 1
    else
        dotfiles-echo-red "Unknown Linux variant - how did we even get here?"
        return 1
    fi
    return 0
}

function _update() {
    _install
}
