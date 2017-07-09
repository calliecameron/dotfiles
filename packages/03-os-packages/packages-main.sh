#!/bin/bash
# Install packages for Mint 18

sudo add-apt-repository -y ppa:git-core/ppa &&

# The FreeFileSync ppa doesn't support xenial; add this back later when it does
# sudo add-apt-repository -y ppa:freefilesync/ffs &&

sudo add-apt-repository -y ppa:nilarimogard/webupd8 &&
sudo add-apt-repository -y ppa:stebbins/handbrake-releases &&
sudo apt-get update &&

sudo apt-get -y install asciidoc aspell bash-doc biber bison bison-doc build-essential chktex clang clang-format cmake cppcheck curl dconf-tools devscripts dia dia-shapes dict dictd dict-foldoc dict-gcide dict-jargon dict-wn dtrx emacs24 emacs24-el emacs-goodies-el epstool flex fonts-crosextra-caladea fonts-crosextra-carlito gimp git global gnucash gnucash-docs handbrake-gtk htop hunspell hunspell-en-gb info inotify-tools kdelibs5-plugins kdelibs5-data make-doc laptop-detect man-db manpages-posix manpages-posix-dev markdown mercurial miniupnpc mosh nano ncurses-term netcat-openbsd octave okular openjdk-8-jdk oxygen-icon-theme powertop python-dev python-nemo python-pip python-tk python3-dev python3-pip python3-tk qtbase5-dev realpath ruby scons screen sqlite3 sshfs ssmtp tcl-dev texinfo texlive-full tk-dev tmux transfig tree ttf-ancient-fonts ttf-mscorefonts-installer ubuntu-restricted-extras unoconv valgrind vlc wdiff wmctrl workrave xclip xmlstarlet xsane xsltproc zsh zsh-doc zsh-lovers || exit 1

if is64bit; then
    if ! which google-chrome &>/dev/null; then
        wget -q -O - https://dl.google.com/linux/linux_signing_key.pub | sudo apt-key add - &&
        TMPDIR="$(mktemp -d)" &&
        wget -O "${TMPDIR}/chrome.deb" https://dl.google.com/linux/direct/google-chrome-stable_current_amd64.deb &&
        sudo dpkg -i "${TMPDIR}/chrome.deb" &&
        rm "${TMPDIR}/chrome.deb" &&
        rmdir "${TMPDIR}" || exit 1
    fi
else
    sudo apt-get -y install chromium-browser || exit 1
fi

sudo -H pip install setuptools &&
sudo -H pip3 install setuptools &&
sudo -H pip install pyinotify pylint pytimeparse future gitpython &&
sudo -H pip3 install pyinotify pylint pytimeparse future gitpython &&

sudo gem install serve &&

fc-cache
