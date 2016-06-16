function _can-install() {
    ([ "${DOTFILES_OS}" = 'linux' ] &&
    [ ! -z "${DOTFILES_LINUX_VARIANT}" ] &&
    [ ! -z "${DOTFILES_CAN_SUDO}" ]) ||
    [ "${DOTFILES_OS}" = 'cygwin' ]
}

function _install() {
    chmod go-rwx ~ || return 1

    if [ "${DOTFILES_OS}" = 'linux' ]; then
        sudo apt-get update &&
        sudo apt-get upgrade &&
        sudo apt-get dist-upgrade || return 1

        if [ "${DOTFILES_LINUX_VARIANT}" = 'main' ]; then
            if lsb_release -a | grep rosa &>/dev/null; then
                "${PACKAGE_CONF_DIR}/packages-main-core.sh" &&
                "${PACKAGE_CONF_DIR}/packages-main-gui.sh" || return 1
            else
                "${PACKAGE_CONF_DIR}/packages-main.sh" || return 1
            fi
        elif [ "${DOTFILES_LINUX_VARIANT}" = 'android' ]; then
            "${PACKAGE_CONF_DIR}/packages-android-linux.sh" || return 1
        elif [ "${DOTFILES_LINUX_VARIANT}" = 'pi' ]; then
            "${PACKAGE_CONF_DIR}/packages-pi.sh" || return 1
        else
            echo-red "Unknown Linux variant - how did we even get here?"
            return 1
        fi
    elif [ "${DOTFILES_OS}" = 'cygwin' ]; then
        "${PACKAGE_CONF_DIR}/packages-cygwin.sh"
    else
        echo-red "Unknown OS - how did we even get here?"
        return 1
    fi
    return 0
}

function _update() {
    _install

    if [ "${DOTFILES_OS}" = 'cygwin' ]; then
        echo-blue "Use the Cygwin GUI installer to check for updates."
    fi
}
