function _can-install() {
    os linux &&
    linux-variant main &&
    can-sudo
}

function _install() {
    # Virtualbox
    echo deb https://download.virtualbox.org/virtualbox/debian bionic contrib | sudo tee /etc/apt/sources.list.d/virtualbox.list &&
    wget -q https://www.virtualbox.org/download/oracle_vbox_2016.asc -O- | sudo apt-key add - &&
    sudo apt-get update &&
    sudo apt-get -y install virtualbox-6.0 &&
    sudo adduser "$(id -un)" vboxusers &&

    # Vagrant
    sudo apt-get -y install vagrant
}
