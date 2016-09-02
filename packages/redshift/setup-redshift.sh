#!/bin/bash

source "${DOTFILES_BASH_COMMON}" &&

sudo apt-get -y install geoclue-2.0 redshift-gtk || exit 1

if [ ! -e "${HOME}/.config/redshift.conf" ]; then
    ln -s "${THIS_DIR}/redshift-mint-18.conf" "${HOME}/.config/redshift.conf" || exit 1
fi

mkdir -p "${HOME}/.config/autostart" &&
cp '/usr/share/applications/redshift-gtk.desktop' "${HOME}/.config/autostart/redshift-gtk.desktop"
