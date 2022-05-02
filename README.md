# Dotfiles

Configurations for Zsh, Emacs, etc.

Works on Linux Mint 20.1 Cinnamon and Raspbian Stretch. Also installs packages
through the package manager and does extensive customisation, as well as setting
up dotfiles. The basic shell parts that don't need root should work on any
recent Ubuntu-like system.

## Install

1. `sudo apt-get install git`.
2. `git clone git@github.com:calliecameron/dotfiles ~/.dotfiles`, or using
HTTPS, `git clone https://github.com/calliecameron/dotfiles ~/.dotfiles`.
3. `~/.dotfiles/install.sh`. This will backup and replace any existing dotfiles.
4. Log out and log back in (don't just start a new shell!).
5. Start a shell and follow the instructions. You may need to log out and in
again several times until all the packages are installed.

If you set `DOTFILES_PRIVATE_REPO` in '~/.dotfiles-variables.sh' to the URL of a
git repository, it will be cloned into 'private' alongside this readme, and any
packages in a 'packages' directory within it will be loaded along with the
packages in the main repository (see [](packages/README.md) for details of the
package format). I use this for anything I don't want to keep in a
publicly-visible repository.
