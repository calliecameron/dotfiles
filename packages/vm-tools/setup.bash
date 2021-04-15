function _can-install() {
    os linux &&
    linux-variant main &&
    can-sudo
}

function _install() {
    # Virtualbox
    echo deb https://download.virtualbox.org/virtualbox/debian focal contrib | sudo tee /etc/apt/sources.list.d/virtualbox.list &&
    wget -q https://www.virtualbox.org/download/oracle_vbox_2016.asc -O- | sudo apt-key add - &&
    sudo apt-get update &&
    sudo apt-get -y install virtualbox-6.1 &&
    sudo adduser "$(id -un)" vboxusers &&

    # Vagrant
    local VAGRANT_VERSION='2.2.15'
    local DEB_FILE="vagrant_${VAGRANT_VERSION}_x86_64.deb"
    local SHA_FILE="vagrant_${VAGRANT_VERSION}_SHA256SUMS"
    local SIG_FILE="vagrant_${VAGRANT_VERSION}_SHA256SUMS.sig"
    local TMP_DIR
    TMP_DIR="$(mktemp -d)" &&
    wget -O "${TMP_DIR}/${DEB_FILE}" "https://releases.hashicorp.com/vagrant/${VAGRANT_VERSION}/${DEB_FILE}" &&
    wget -O "${TMP_DIR}/${SHA_FILE}" "https://releases.hashicorp.com/vagrant/${VAGRANT_VERSION}/${SHA_FILE}" &&
    wget -O "${TMP_DIR}/${SIG_FILE}" "https://releases.hashicorp.com/vagrant/${VAGRANT_VERSION}/${SIG_FILE}" &&
    cd "${TMP_DIR}" &&
    sha256sum --ignore-missing -c "${SHA_FILE}" &&
    gpg --recv-keys 51852D87348FFC4C &&
    gpg --verify "${SIG_FILE}" "${SHA_FILE}" &&
    sudo dpkg -i "${DEB_FILE}" &&
    rm -rf "${TMP_DIR}"
}
