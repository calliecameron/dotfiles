#!/bin/bash
# Install Android-specific packages

sudo apt-get -y install software-properties-common &&

sudo add-apt-repository -y ppa:git-core/ppa &&
sudo apt-get update &&

sudo apt-get -y install aspell bash-doc bison bison-doc build-essential chktex clang cmake curl dict dtrx emacs24 emacs24-el emacs-goodies-el flex info inotify-tools git global htop language-pack-en-base make-doc man-db markdown mercurial nano ncurses-term netcat-openbsd python-pip python3-pip realpath scons screen sqlite3 ssh-askpass sshfs ssmtp texinfo tmux tree valgrind xterm zsh zsh-doc zsh-lovers

cat <<EOF | sudo tee /etc/default/locale >/dev/null
LANG="en_GB.UTF-8"
LANGUAGE="en_GB:en"
EOF
