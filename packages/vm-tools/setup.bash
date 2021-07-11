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
    curl -fsSL https://apt.releases.hashicorp.com/gpg | sudo apt-key add - &&
    sudo apt-add-repository "deb [arch=amd64] https://apt.releases.hashicorp.com focal main" &&
    sudo apt-get update &&
    sudo apt-get install vagrant
}
