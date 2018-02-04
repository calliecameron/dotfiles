function _can-install() {
    os linux &&
    linux-variant main &&
    can-sudo
}

function _install() {
    sudo apt-get update &&
    sudo apt-get -y install software-properties-common &&
    sudo apt-add-repository ppa:ansible/ansible &&
    sudo apt-get update &&
    sudo apt-get -y install ansible
}
