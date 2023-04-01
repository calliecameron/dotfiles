#!/bin/bash

source "${DOTFILES_BASH_COMMON}" &&

sudo apt-get -y install openssh-server &&

CONFIG='/etc/ssh/sshd_config' &&

sed "s|@@@@@1@@@@@|$(id -un)|g" < "${THIS_DIR}/sshd-config-template" | sudo tee "${CONFIG}" &>/dev/null &&
sudo chown root:root "${CONFIG}" &&
sudo chmod a=r "${CONFIG}" || exit 1

if dotfiles-yn-n "Open firewall port for SSH server (TCP port 22)?"; then
    port open-at-boot 22 tcp &&
    port open 22 tcp || exit 1
else
    port dont-open-at-boot 22 tcp || exit 1
fi

exit 0
