#!/bin/bash

source "${DOTFILES_BASH_COMMON}" &&

sudo apt-get -y install redshift-gtk || exit 1

if [ ! -e "${HOME}/.config/redshift.conf" ]; then
    ln -s "${THIS_DIR}/redshift.conf" "${HOME}/.config/redshift.conf" || exit 1
fi

mkdir -p "${HOME}/.config/autostart" &&
cp '/usr/share/applications/redshift-gtk.desktop' "${HOME}/.config/autostart/redshift-gtk.desktop"
