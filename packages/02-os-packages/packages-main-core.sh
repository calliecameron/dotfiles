#!/bin/bash
# Install the basic packages for a dotfiles system

sudo add-apt-repository -y ppa:git-core/ppa &&
sudo apt-get update &&

sudo apt-get -y install asciidoc aspell bash-doc bison bison-doc build-essential chktex clang cmake cppcheck curl devscripts dict dtrx flex info inotify-tools git global htop hunspell latex-beamer make-doc man-db markdown mercurial miniupnpc nano ncurses-term netcat-openbsd openjdk-7-jdk python-pip python3-pip realpath ruby scons screen sshfs ssmtp texinfo texlive texlive-extra-utils texlive-latex-extra tmux tree valgrind wdiff xmlstarlet xsltproc zsh zsh-doc zsh-lovers &&

sudo pip install pylint pytimeparse future gitpython &&
sudo pip3 install pylint pytimeparse future gitpython &&

sudo gem install serve
