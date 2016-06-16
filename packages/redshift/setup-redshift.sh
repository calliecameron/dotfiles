#!/bin/bash

source "${DOTFILES_BASH_COMMON}" &&

if lsb_release -a | grep rosa &>/dev/null; then
    sudo apt-get -y install redshift-gtk || exit 1
else
    sudo apt-get -y install geoclue-2.0 redshift-gtk || exit 1
fi

if [ ! -e "${HOME}/.config/redshift.conf" ]; then
    if lsb_release -a | grep rosa &>/dev/null; then
        ln -s "${THIS_DIR}/redshift.conf" "${HOME}/.config/redshift.conf" || exit 1
    else
        ln -s "${THIS_DIR}/redshift-mint-18.conf" "${HOME}/.config/redshift.conf" || exit 1
    fi
fi

mkdir -p "${HOME}/.config/autostart" &&
cp '/usr/share/applications/redshift-gtk.desktop' "${HOME}/.config/autostart/redshift-gtk.desktop"
