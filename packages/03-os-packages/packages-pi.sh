#!/bin/bash
# Install the basic packages for Raspbian Stretch

sudo apt-get -y install git htop mercurial nano realpath tmux tree zsh zsh-doc || exit 1

if [ ! -z "${DOTFILES_PI_INTERACTIVE}" ]; then
    sudo apt-get -y install asciidoc aspell bash-doc bison bison-doc build-essential clang clang-format-3.9 cmake cppcheck curl devscripts dict dictd dict-foldoc dict-gcide dict-jargon dict-wn dtrx emacs24 emacs24-el emacs-goodies-el flex global hunspell info inotify-tools make-doc man-db manpages-posix manpages-posix-dev markdown mercurial mosh myspell-en-gb ncurses-term netcat-openbsd python-dev python-pip python3-dev python3-pip qtbase5-dev ruby scons screen sqlite3 texinfo tree unoconv valgrind wdiff &&

    sudo -H pip install setuptools &&
    sudo -H pip3 install setuptools &&
    sudo -H pip install pyinotify pylint pytimeparse future gitpython &&
    sudo -H pip3 install pyinotify pylint pytimeparse future gitpython &&

    sudo gem install serve || exit 1
fi
