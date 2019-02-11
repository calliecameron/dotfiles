#!/bin/bash

source "${DOTFILES_BASH_COMMON}" &&

sudo apt-get -y install python-gpg nemo-dropbox || exit 1

if yn-n "Open firewall port for Dropbox LAN sync (TCP port 17500)?"; then
    port open-at-boot 17500 tcp &&
    port open 17500 tcp || exit 1
else
    port dont-open-at-boot 17500 tcp || exit 1
fi

exit 0
