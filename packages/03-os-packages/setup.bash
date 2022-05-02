function _can-install() {
    os linux && known-linux-variant && can-sudo
}

function _install() {
    chmod go-rwx ~ || return 1

    sudo apt-get update &&
    sudo apt-get upgrade &&
    sudo apt-get dist-upgrade || return 1

    if linux-variant main; then
        "${PACKAGE_CONF_DIR}/packages-main.sh" || return 1
    elif linux-variant pi; then
        "${PACKAGE_CONF_DIR}/packages-pi.sh" || return 1
    else
        echo-red "Unknown Linux variant - how did we even get here?"
        return 1
    fi
    return 0
}

function _update() {
    _install
}
