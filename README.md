Dotfiles
========

Configurations for Zsh, Emacs, and lots of other stuff.

Currently set up to work with Linux Mint 19.1 Cinnamon, Raspbian Stretch, and
Cygwin. On these systems (especially Mint) it installs things through the
package manager and does extensive customisation, in addition to just setting up
dotfiles; the basic shell parts that don't need root, however, should work fine
on any recent Ubuntu-like system.


Install
-------

1. `sudo apt-get install git`
2. `git clone git@github.com:calliecameron/dotfiles ~/.dotfiles`, or using
HTTPS, `git clone https://github.com/calliecameron/dotfiles ~/.dotfiles`
3. `~/.dotfiles/install.sh`. This will backup and replace any existing dotfiles.
4. Log out and log back in (don't just start a new shell!).
5. Start a shell and follow the instructions. You may need to log out and in
again several times until all the packages are installed.

For [Cygwin](misc/cygwin.md), there are other things to do first; see the
appropriate readme, then come back here.

If you set `DOTFILES_PRIVATE_REPO` in '~/.dotfiles-variables.sh' to the URL of a
git repository, it will be cloned into 'private' alongside this readme, and any
packages in a 'packages' directory within it will be loaded along with the
packages in the main repository (see [](packages/README.md) for details of the
package format). I use this for anything I don't want to keep in a
publicly-visible repository.
