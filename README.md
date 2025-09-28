# Dotfiles

[![template](https://img.shields.io/badge/template-calliecameron%2Fcopier--template-brightgreen)](https://github.com/calliecameron/copier-template)
[![pre-commit](https://img.shields.io/badge/pre--commit-enabled-brightgreen?logo=pre-commit)](https://github.com/pre-commit/pre-commit)
[![CI](https://github.com/calliecameron/dotfiles/actions/workflows/ci.yml/badge.svg)](https://github.com/calliecameron/dotfiles/actions/workflows/ci.yml)

Configurations for Zsh, Emacs, etc.

Tested on Linux Mint 22.1 Cinnamon, but should work on any recent Ubuntu-like
system. Also installs packages through the package manager and does extensive.
customisation.

## Install

1. `sudo apt-get install git`.
2. `git clone https://github.com/calliecameron/dotfiles ~/.dotfiles`.
3. `~/.dotfiles/install.sh`. This will backup and replace any existing dotfiles.
4. Log out and log back in (don't just start a new shell!).
5. Start a shell and follow the instructions. You may need to log out and in
   again several times until all the packages are installed.

If you set `DOTFILES_PRIVATE_REPO` in `~/.dotfiles-variables.sh` to the URL of a
git repository, it will be cloned into `~/.dotfiles.d/private`, and any packages
in a 'packages' directory within it will be loaded along with the packages in
the main repository (see [PACKAGES.md](PACKAGES.md)).
