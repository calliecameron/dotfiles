#!/bin/bash
# Install GUI-specific packages for Mint 17.3

sudo add-apt-repository -y ppa:freefilesync/ffs &&
sudo add-apt-repository -y ppa:nilarimogard/webupd8 &&
sudo add-apt-repository -y ppa:stebbins/handbrake-releases &&
sudo apt-get update &&

sudo apt-get -y install biber dconf-tools dia dia-shapes dictd dict-foldoc dict-gcide dict-jargon dict-wn emacs24 emacs24-el emacs-goodies-el epstool fonts-crosextra-caladea fonts-crosextra-carlito freefilesync gimp gnucash gnucash-docs handbrake-gtk kdelibs5-plugins kdelibs5-data manpages-posix manpages-posix-dev mosh octave okular oxygen-icon-theme python-nemo qtbase5-dev sqlite3 texlive-full transfig ttf-ancient-fonts ttf-mscorefonts-installer ubuntu-restricted-extras unoconv wmctrl workrave xclip xsane || exit 1

if is64bit -q; then
    if ! which google-chrome &>/dev/null; then
        wget -q -O - https://dl.google.com/linux/linux_signing_key.pub | sudo apt-key add - &&
        TMPDIR="$(mktemp -d)" &&
        wget -O "${TMPDIR}/chrome.deb" https://dl.google.com/linux/direct/google-chrome-stable_current_amd64.deb &&
        sudo dpkg -i "${TMPDIR}/chrome.deb" &&
        rm "${TMPDIR}/chrome.deb" &&
        rmdir "${TMPDIR}" || exit 1
    fi
else
    sudo apt-get install chromium-browser || exit 1
fi

sudo pip install pyinotify &&
sudo pip3 install pyinotify &&

fc-cache
