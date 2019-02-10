Common dirs
===========

When on Linux dual-booted with Windows, you can set some environment
variables in `~/.dotfiles-variables.sh` to make accessing the Windows
partition more convenient; these paths will be symlinked into home
(only works if the Windows partition is mounted).

- `WINROOT`: Windows partition's mount point
- `WINHOME`: Windows user's home directory
- `WINDOWNLOADS`: Windows user's downloads directory
- `CYGHOME`: Windows user's Cygwin home directory

