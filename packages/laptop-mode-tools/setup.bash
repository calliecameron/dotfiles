function _can-install() {
    os linux &&
    linux-variant main &&
    can-sudo &&
    type laptop-detect &>/dev/null &&
    laptop-detect
}

function _install() {
    sudo apt-get install laptop-mode-tools
}
