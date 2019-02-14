#!/bin/bash
# Install packages for Mint 19

sudo apt-get update &&
sudo apt-get autoremove pix hexchat samba-common samba-libs thunderbird transmission-gtk &&

sudo apt-get -y install apparmor-profiles apparmor-utils curl fonts-crosextra-caladea fonts-crosextra-carlito git inotify-tools laptop-detect mercurial nano ncurses-term netcat-openbsd powertop screen sqlite3 tree ttf-ancient-fonts ttf-mscorefonts-installer ubuntu-restricted-extras xclip zsh zsh-doc &&

sudo rm -f /etc/sudoers.d/0pwfeedback
fc-cache
