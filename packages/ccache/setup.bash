function _can-install() {
    (os linux && linux-variant main android && can-sudo) ||
    os cygwin
}

function _install() {
    if os linux; then
        sudo apt-get -y install ccache || return 1
    elif os cygwin; then
        apt-cyg install ccache || return 1
    else
        echo-red "Unknown OS"
        return 1
    fi
}

function _update() {
    _install
}
