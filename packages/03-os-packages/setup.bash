function _can-install() {
    (os linux && known-linux-variant && can-sudo) ||
    os cygwin
}

function _install() {
    chmod go-rwx ~ || return 1

    if os linux; then
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
    elif os cygwin; then
        "${PACKAGE_CONF_DIR}/packages-cygwin.sh"
    else
        echo-red "Unknown OS - how did we even get here?"
        return 1
    fi
    return 0
}

function _update() {
    _install

    if os cygwin; then
        echo-blue "Use the Cygwin GUI installer to check for updates."
    fi
}
