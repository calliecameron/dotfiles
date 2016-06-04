#!/bin/bash

source "${DOTFILES_BASH_COMMON}" &&

curl -s https://syncthing.net/release-key.txt | sudo apt-key add - &&
echo deb http://apt.syncthing.net/ syncthing release | sudo tee /etc/apt/sources.list.d/syncthing-release.list &&
sudo add-apt-repository -y ppa:nilarimogard/webupd8 &&
sudo apt-get update &&

sudo apt-get -y install syncthing syncthing-gtk || exit 1

if yn-n "Open firewall port for Syncthing (TCP 22000 and UDP 21027)?"; then
    port open-at-boot 22000 tcp &&
    port open-at-boot 21027 udp &&
    port open 22000 tcp &&
    port open 21027 udp || exit 1
else
    port dont-open-at-boot 22000 tcp &&
    port dont-open-at-boot 21027 udp || exit 1
fi

exit 0
