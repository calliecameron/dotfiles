Cygwin
======

1. Install Cygwin to 'C:\cygwin64', and install the package 'git'.
2. Clone the dotfiles repo and install as usual.


Set the default shell to zsh
----------------------------

After installing, edit the start menu and desktop shortcuts for the
Cygwin 64 Terminal (which should point to mintty), and the replace
the `-` at the end of the arguments with `/usr/bin/zsh -l -i`.


Opening from Windows Explorer
-----------------------------

From an administrative shell, run:

    chere -i -t mintty -s zsh -o '-i /Cygwin-Terminal.ico'
