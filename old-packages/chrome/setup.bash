function _can-install() {
    dotfiles-linux-variant main && dotfiles-can-sudo
}

function _install() {
    if ! which google-chrome &>/dev/null; then
        wget -q -O - https://dl.google.com/linux/linux_signing_key.pub | sudo apt-key add - &&
        TMPDIR="$(mktemp -d)" &&
        wget -O "${TMPDIR}/chrome.deb" https://dl.google.com/linux/direct/google-chrome-stable_current_amd64.deb &&
        sudo dpkg -i "${TMPDIR}/chrome.deb" &&
        rm "${TMPDIR}/chrome.deb" &&
       rmdir "${TMPDIR}" || exit 1
    fi
}
