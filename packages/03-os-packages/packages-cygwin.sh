#!/bin/bash
# Install the basic packages for a dotfiles system

if ! which apt-cyg &>/dev/null; then
    lynx -source rawgit.com/transcode-open/apt-cyg/master/apt-cyg > apt-cyg &&
    install apt-cyg /bin || exit 1
fi

apt-cyg install asciidoc aspell aspell-en autoconf automake biber bison chere clang cmake cppcheck curl epstool flex info gcc-core gcc-g++ git hunspell make mercurial mosh nano nc6 octave openssh python python-pygments python-setuptools python3 python3-setuptools scons screen sqlite3 texinfo texlive tmux transfig tree wdiff wget xmlstarlet zsh
